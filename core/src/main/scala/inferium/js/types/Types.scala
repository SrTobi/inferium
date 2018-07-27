package inferium.js.types

import java.security.Signature

import escalima.ast.MethodKind.constructor
import inferium.Unifiable
import inferium.dataflow.calls.SignatureCall
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice.{Primitive, _}
import inferium.typescript._
import inferium.utils.Utils
import javax.print.attribute.standard.MediaSize.Other
import ujson.Js

import scala.collection.mutable
import scala.util.Try

// noinspection ScalaFileName
object js {
    class LocGen {
        private var needLoc = true
        private val it =
            Stream
                .continually(if(needLoc) Location() else new LocGen)
                .iterator

        def loc(): Location = {
            needLoc = true
            val l = it.next()
            l.asInstanceOf[Location]
        }

        def gen(): LocGen = {
            needLoc = false
            val gen = it.next()
            gen.asInstanceOf[LocGen]
        }
    }

    object InstType extends Enumeration {
        type Type = Value
        val Prototype, Paragon = Value
    }

    class Instantiator(locations: Stream[Location], val thisEntity: Entity, val instantiated: mutable.Map[(Type, InstType.Type, Seq[Entity]), ObjectLike], val makeAbstract: Boolean = true) {

        val locs: Iterator[Location] = locations.iterator

        def newLoc(): Location = locs.next()
    }


    sealed abstract class Type {
        def unionWith(other: Type): Type = Type.union(this, other)
        def intersectWith(other: Type): Type = Type.intersection(this, other)

        def matches(arg: Entity): Boolean
        def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity

    }

    object Type {
        def union(ty: Type, rest: Type*): Type = union(ty +: rest)(Map.empty)
        def union(types: Traversable[Type])(rec: Map[CompoundType, Instantiate]): Type = {
            var obj: Option[CompoundType] = None
            var selfInst = new Instantiate()
            var hasAny = false

            def flat(tys: Traversable[Type]): Seq[Type] = tys.toSeq flatMap {
                case NeverType => Seq.empty
                case inst: Instantiate =>
                    assert(!inst.typeArguments.exists(_.isInstanceOf[GenericType]))
                    if (inst.typeArguments.nonEmpty || inst.target == null)
                        Seq(inst)
                    else
                        flat(Seq(inst.target))

                case AnyType =>
                    hasAny = true
                    Seq.empty

                case ty: AtomarType => Seq(ty)
                case union: UnionType => flat(union.types)
                case intersection: IntersectionType => Seq(intersection)
                case comp: CompoundType =>
                    rec.get(comp) match {
                        case Some(r) => Seq(r)
                        case None =>
                            obj match {
                                case Some(other) =>
                                    obj = Some(comp.union(other,rec + (comp -> selfInst)))
                                case None =>
                                    obj = Some(comp)
                            }
                            Seq.empty
                    }
                case probe: ProbeType =>
                    Seq(probe)
                case tuple: TupleType =>
                    ???
                case _: GenericType =>
                    ???
            }

            val atoms = flat(types)
            obj foreach { selfInst.target = _ }

            if (hasAny)
                return AnyType

            UnionType(atoms ++ obj.toSeq)
        }

        def intersection(ty: Type, rest: Type*): Type = intersection(ty +: rest)(Map.empty)
        def intersection(types: Traversable[Type])(rec: Map[CompoundType, Instantiate]): js.Type = {
            var obj: Option[CompoundType] = None
            var selfInst = new Instantiate()
            var hasAny = false

            def flat(tys: Traversable[Type]): Seq[Type] = tys.toSeq flatMap {
                case NeverType => Seq.empty
                case AnyType =>
                    hasAny = true
                    Seq(AnyType)
                case inst: Instantiate =>
                    assert(!inst.typeArguments.exists(_.isInstanceOf[GenericType]))
                    if (inst.typeArguments.nonEmpty || inst.target == null)
                        Seq(inst)
                    else
                        flat(Seq(inst.target))

                case ty: AtomarType => Seq(ty)
                case union: UnionType =>
                    Seq(union)
                case intersection: IntersectionType =>
                    flat(intersection.types)
                case comp: CompoundType =>
                    rec.get(comp) match {
                        case Some(r) => Seq(r)
                        case None =>
                            obj match {
                                case Some(other) =>
                                    obj = Some(comp.intersect(other,rec + (comp -> selfInst)))
                                case None =>
                                    obj = Some(comp)
                            }
                            Seq.empty
                    }
                case probe: ProbeType =>
                    Seq(probe)
                case tuple: TupleType =>
                    ???
                case _: GenericType =>
                    ???
            }

            val atoms = flat(types)
            obj foreach { selfInst.target = _ }

            if (hasAny) {
                return AnyType
            }

            IntersectionType(atoms ++ obj.toSeq )
        }



    }

    sealed abstract class AtomarType extends Type

    case object AnyType extends AtomarType {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity = AnyEntity

        override def matches(arg: Entity): Boolean = true
    }

    sealed abstract class Primitive extends AtomarType

    case object NeverType extends Primitive {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= NeverValue

        override def matches(arg: Entity): Boolean = false
    }
    case object UndefinedType extends Primitive {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= UndefinedValue

        override def matches(arg: Entity): Boolean = arg == UndefinedValue
    }
    case object NullType extends Primitive {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= NullValue

        override def matches(arg: Entity): Boolean = arg == NullValue
    }

    sealed abstract class BooleanType extends Primitive
    case object BooleanType extends BooleanType {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= BoolValue

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[BoolValue]
    }

    case object TrueType extends BooleanType {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= TrueValue

        override def matches(arg: Entity): Boolean = arg == TrueValue
    }
    case object FalseType extends BooleanType {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= FalseValue

        override def matches(arg: Entity): Boolean = arg == FalseValue
    }

    case object NumberType extends Primitive {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= NumberValue

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[NumberValue]
    }

    sealed abstract class StringType extends Primitive
    case object StringType extends StringType {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= StringValue

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[StringValue]
    }

    final case class LiteralType(value: String) extends StringType {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= SpecificStringValue(value)

        override def matches(arg: Entity): Boolean = arg match {
            case SpecificStringValue(otherValue) => this.value == otherValue
            case _ => false
        }
    }

    case object ThisType extends AtomarType {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= {
            val thisEntity = instantiator.thisEntity
            assert(thisEntity != NeverValue)
            thisEntity
        }

        override def matches(arg: Entity): Boolean = false
    }

    final class TupleType extends Type {
        var members: Seq[Type] = _

        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= {
            val arrayLoc = instantiator.newLoc()
            val array = heap.allocArray(arrayLoc, makeAbstract = true)
            ArrayUtils.fillAbstractArray(array, members.map { _.instantiate(heap, instantiator, substitutions) }, None, heap)
            array
        }

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[ObjectLike]
    }

    case object ObjectType extends AtomarType {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= {
            AnyEntity
        }

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[ObjectLike]
    }

    final class ProbeType(val probe: ProbeEntity) extends Type {
        override def hashCode(): Int = probe.hashCode()

        override def equals(o: scala.Any): Boolean = o match {
            case o: ProbeType => probe == o.probe
            case _ => false
        }

        override def matches(arg: Entity): Boolean = true
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity = probe

        def bound: Type = {
            val readProps = probe._reads.map {
                case (name, ty) => name -> Property(name, ty)
            }.toMap

            val writeProps = probe._writes.map {
                case (name, ty) => name -> Property(name, ty, optional = true)
            }.toMap

            val properties = Utils.mergeMaps(readProps, writeProps) {
                case (rp, wp) => Property(rp.name, UnionType(rp.ty, wp.ty))
            }

            val signature = probe._calls.map {
                case (ret, (args, _)) =>
                    Overload(Seq.empty, args map { Param("_", _, optional = true) }, new ProbeType(ret))
            }.toSeq
            val constructor = probe._constructors.map {
                case (ret, args) =>
                    Overload(Seq.empty, args map { Param("_", _, optional = true) }, new ProbeType(ret))
            }.toSeq


            val obj = if (readProps.isEmpty && writeProps.isEmpty && properties.isEmpty && signature.isEmpty && constructor.isEmpty) {
                NeverType
            } else {
                val obj = new CompoundType(None, false)

                obj._resolve(Seq.empty, Seq.empty, signature, constructor, properties.values.toSeq)
                obj
            }

            val union = UnionType(obj, probe._usedAs)
            if (union == NeverType) {
                AnyType
            } else {
                union
            }
        }
    }


    final class GenericType(/*val name: String, */) extends Type {
        var constraint: Option[Type] = None

        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= {
            substitutions.getOrElse(this, AnyEntity)
        }

        override def matches(arg: Entity): Boolean = constraint.exists(_.matches(arg))
    }

    final class UnionType extends Type {
        var types: Set[Type] = _

        override def equals(other: scala.Any): Boolean = other match {
            case other: UnionType => this.types == other.types
            case _ => false
        }
        override def hashCode: Int = types.hashCode()
        override def toString: String = types.mkString(" | ")

        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= {
            Entity.unify(types map { _.instantiate(heap, instantiator, substitutions) })
        }

        override def matches(arg: Entity): Boolean = true
    }

    object UnionType {
        def apply(types: Type*): Type = apply(types.toSet)
        def apply(types: TraversableOnce[Type]): Type = {
            val set = types.flatMap {
                case NeverType => Seq.empty
                case u: UnionType => u.types
                case any => Seq(any)
            }.toSet

            if (set.size == 1)
                set.head
            else if (set.isEmpty) {
                NeverType
            } else {
                val res = new UnionType
                res.types = set
                res
            }
        }
    }


    final class IntersectionType extends Type {
        var types: Set[Type] = _

        override def equals(other: scala.Any): Boolean = other match {
            case other: IntersectionType => this.types == other.types
            case _ => false
        }
        override def hashCode: Int = types.hashCode()
        override def toString: String = types.mkString(" & ")

        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= {
            Entity.unify(types map { _.instantiate(heap, instantiator, substitutions) })
        }

        override def matches(arg: Entity): Boolean = true
    }

    object IntersectionType {
        def apply(types: Type*): Type = apply(types.toSet)
        def apply(types: TraversableOnce[Type]): Type = {
            val set = types.flatMap {
                case NeverType => Seq.empty
                case u: IntersectionType => u.types
                case any => Seq(any)
            }.toSet

            if (set.size == 1)
                set.head
            else if (set.isEmpty) {
                NeverType
            } else  {
                val res = new IntersectionType
                res.types = set
                res
            }
        }
    }

    val VoidType: Type = UnionType(UndefinedType, NullType)

    sealed trait ClassLike extends Type {
        def isClass: Boolean

        def constructor: Signature

        def ownProperties: Map[String, Property]
        def allProperties: Map[String, Property]
        def instProperties: Map[String, Property]

        def instantiatePrototype(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): ObjectLike
    }

    final class Instantiate extends AtomarType with ClassLike {
        // todo: check if typeArguments has the same length as the target's typeParameters
        var typeArguments: Seq[Type] = _
        var target: CompoundType = _

        override def constructor: Signature = target.constructor
        override def ownProperties: Map[String, Property] = target.ownProperties
        override def allProperties: Map[String, Property] = target.allProperties
        override def instProperties: Map[String, Property] = target.instProperties

        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= {
            // todo: respect default type parameter
            val newSubstitution = instantiateSubstitution(heap, instantiator, substitutions)

            target.instantiate(heap, instantiator, newSubstitution)
        }

        override def instantiatePrototype(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): ObjectLike = {
            // todo: respect default type parameter
            val newSubstitution = instantiateSubstitution(heap, instantiator, substitutions)

            target.instantiatePrototype(heap, instantiator, newSubstitution)
        }

        private def instantiateSubstitution(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]) = {
            assert(typeArguments.length == target.typeParameter.length)
            (target.typeParameter zip typeArguments).map {
                case (generic, ty) => (generic, ty.instantiate(heap, instantiator, substitutions))
            }.toMap
        }

        override def matches(arg: Entity): Boolean = true

        override def isClass: Boolean = target.isClass
    }

    object Instantiate {
        def apply(target: CompoundType, typeArguments: Seq[Type] = Seq.empty): Instantiate = {
            val inst = new Instantiate()
            inst.target = target
            inst.typeArguments = typeArguments
            inst
        }
    }

    final class CompoundType(val name: Option[String], var isClass: Boolean = false) extends Type with ClassLike {
        lazy val defaultConstructor = {
            assert(isClass)
            val o = new Overload(Seq.empty, Seq.empty)
            o.returnType = this
            Seq(o)
        }

        var bases: Seq[ClassLike] = _
        var typeParameter: Seq[GenericType] = _
        var signature: Signature = _
        var constructor: Signature = _

        private def baseClasses = bases collect { case c: CompoundType if c.isClass => c }
        def baseClass: Option[CompoundType] = {
            assert(baseClasses.size <= 1)
            baseClasses.headOption
        }

        def baseInterfaces: Seq[CompoundType] = bases collect { case c: CompoundType if !c.isClass => c}

        var ownProperties: Map[String, Property] = _
        lazy val allProperties: Map[String, Property] = Utils.mergeMaps(bases map {_.allProperties}: _*)((a, b) => a) ++ ownProperties
        lazy val instProperties: Map[String, Property] = Utils.mergeMaps(baseInterfaces map {_.instProperties}: _*)((a, b) => a) ++ ownProperties

        def callable: Boolean = signature.nonEmpty
        def constructable: Boolean = constructor.nonEmpty

        private[js] def _resolve(bases: Seq[ClassLike],
                                 typeParameter: Seq[GenericType],
                                 signature: Signature,
                                 constructor: Signature,
                                 properties: Seq[Property]): Unit = {
            this.bases = bases
            this.typeParameter = typeParameter
            this.signature = signature
            this.constructor = constructor
            this.ownProperties = properties.map(p => p.name -> p).toMap
            assert(this.ownProperties.size == properties.size)
        }


        def addProperties(obj: ObjectLike, heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Unit = {

            instProperties foreach {
                case (name, prop) =>
                    val value = prop.ty.instantiate(heap, instantiator, substitutions)
                    // todo: writable?
                    if (instantiator.makeAbstract || obj.isInstanceOf[FunctionEntity]) {
                        heap.setProperty(obj, name, AbstractProperty.defaultWriteToObject(value, prop.optional))
                    } else {
                        val loc = heap.setValue(Location(), value)
                        heap.setProperty(obj, name, ConcreteProperty.defaultWriteToObject(Set(loc), prop.optional))
                    }
            }

        }

        def instantiatePrototype(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): ObjectLike = {
            import instantiator.instantiated
            assert(isClass)

            val instKey = (this, InstType.Prototype, typeParameter map { substitutions(_) })

            instantiated.get(instKey) match {
                case Some(existing) =>
                    return existing
                case _ =>
            }

            val prototypeLoc = instantiator.newLoc()

            lazy val basePrototype = baseClass match {
                case Some(base) =>
                    base.instantiatePrototype(heap, instantiator, substitutions)
                case _ =>
                    heap.specialObject(SpecialObjects.Object)
            }

            val prototype = heap.allocOrdinaryObject(prototypeLoc, basePrototype, makeAbstract = instantiator.makeAbstract)
            instantiator.instantiated += instKey -> prototype
            prototype
        }

        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): ObjectLike = {

            import instantiator.instantiated

            val instKey = (this, InstType.Paragon, typeParameter map { substitutions(_) })

            instantiated.get(instKey) match {
                case Some(existing) =>
                    return existing
                case _ =>
            }

            val paragonLoc = instantiator.newLoc()

            val isFunctionEntity = callable || constructable
            lazy val prorotype = if (isClass) {
                instantiatePrototype(heap, instantiator, substitutions)
            } else if (isFunctionEntity) {
                heap.specialObject(SpecialObjects.Function)
            } else {
                heap.specialObject(SpecialObjects.Object)
            }

            val paragon = name match {
                case Some("Array") =>
                    heap.allocArray(paragonLoc, makeAbstract = instantiator.makeAbstract)
                case _ =>
                    heap.allocObject(paragonLoc, (loc, ac) => {
                        if (isFunctionEntity) {
                            val callableInfo = SignatureCall.createCallableInfo(name, signature, constructor)
                            new SignatureFunctionEntity(loc, callableInfo)
                        } else
                            OrdinaryObjectEntity(loc)(ac)
                    }, prorotype, makeAbstract = instantiator.makeAbstract || isFunctionEntity)
            }
            instantiator.instantiated += instKey -> paragon

            if (name.contains("Array")) {
                val loc = heap.setValue(Location(), substitutions(typeParameter.head))
                heap.writeToProperties(paragon, loc, numbersOnly = true, AnyEntity)
            } else if (!isClass) {
                addProperties(paragon, heap, instantiator, substitutions)
            }


            paragon
        }

        //override def toString: String = s"${name.getOrElse("")}{${properties.mkString(", ")}}"
        override def matches(arg: Entity): Boolean = true


        def union(other: CompoundType, rec: Map[CompoundType, Instantiate]): CompoundType = {
            assert(bases.isEmpty && typeParameter.isEmpty)
            assert(other.bases.isEmpty && other.bases.isEmpty)

            if (other == this) {
                this
            } else {
                val result = new CompoundType(None, false)

                val newSignature = this.signature ++ other.signature
                val newConstructor = this.constructor ++ other.constructor
                val newProperties = Utils.mergeMapsWithMapper(ownProperties, other.ownProperties) { _.withOptional } {
                    _.union(_, rec)
                }

                result._resolve(Seq.empty, Seq.empty, newSignature, newConstructor, newProperties.values.toSeq)
                result
            }
        }

        def intersect(other: CompoundType, rec: Map[CompoundType, Instantiate]): CompoundType = {
            assert(bases.isEmpty && typeParameter.isEmpty)
            assert(other.bases.isEmpty && other.bases.isEmpty)

            if (other == this) {
                this
            } else {
                val result = new CompoundType(None, false)

                val newSignature = this.signature ++ other.signature
                val newConstructor = this.constructor ++ other.constructor
                val newProperties = Utils.mergeMaps(ownProperties, other.ownProperties) {
                    _.intersection(_, rec)
                }

                result._resolve(Seq.empty, Seq.empty, newSignature, newConstructor, newProperties.values.toSeq)
                result
            }
        }
    }

    /*object FunctionType {
        def apply(name: String, signature: => Signature): CompoundType = new CompoundType(Option(name), signature, Seq.empty, None)
        def apply(signature: => Signature): CompoundType = FunctionType(null, signature)
    }

    object InterfaceType {
        def apply(properties: => Seq[Property], name: String = null, extend: Type = null): CompoundType = new CompoundType(Some(name), Seq.empty, properties, Option(extend))
    }*/

    final case class Property(name: String, ty: Type, optional: Boolean = false, readonly: Boolean = false) {
        def withOptional: Property = copy(optional = true)

        def union(other: Property, rec: Map[CompoundType, Instantiate]): Property = {
            assert(name == other.name)
            Property(
                name,
                Type.union(Seq(ty, other.ty))(rec),
                optional || other.optional,
                readonly || other.readonly
            )
        }

        def intersection(other: Property, rec: Map[CompoundType, Instantiate]): Property = {
            assert(name == other.name)
            Property(
                name,
                Type.union(Seq(ty, other.ty))(rec),
                optional && other.optional,
                readonly && other.readonly
            )
        }
    }

    type Signature = Seq[Overload]

    final class Overload(val generics: Seq[GenericType], val params: Seq[Param]) {
        var returnType: Type = _
    }

    object Overload {
        def apply(generics: Seq[GenericType], params: Seq[Param], returnType: Type): Overload = {
            val overload = new Overload(generics, params)
            overload.returnType = returnType
            overload
        }

        def union(other: Property, rec: Map[CompoundType, Instantiate]): Overload = {
            ???
        }
    }

    class Param(val name: String, val optional: Boolean) {
        var ty: Type = _
    }

    object Param {
        def apply(name: String, ty: Type, optional: Boolean): Param = {
            val param = new Param(name,optional)
            param.ty = ty
            param
        }
    }

    def from(entity: Entity, heap: Heap.Mutator, objs: mutable.Map[(Location, Boolean), CompoundType] = mutable.Map.empty): Type = entity.normalized(heap) match {
        case NeverValue => NeverType
        case NullValue => NullType
        case UndefinedValue => UndefinedType
        case _: BoolValue => BooleanType
        case _: NumberValue => NumberType
        case StringValue => StringType
        case SpecificStringValue(string) => LiteralType(string)
        case union: UnionValue => UnionType(union.entities.iterator.map{from(_, heap, objs)})
        case probe: ProbeEntity => new ProbeType(probe)
        case obj: ObjectLike =>
            val isConcrete = heap.isConcreteObject(obj)
            val key = (obj.loc, isConcrete)
            objs.get(key) foreach {
                cty =>
                    return cty
            }

            val cty = new CompoundType(None)
            objs += (key -> cty)

            var props = Map.empty[String, (Boolean, IniEntity)]



            /*def onlyClassLikes(tys: Seq[Type]): Seq[ClassLike] = tys flatMap {
                case union: UnionType => onlyClassLikes(union.types.toSeq)
                case classLike: ClassLike => Seq(classLike)
                case _ => Seq.empty
            }*/

            //val protoType = onlyClassLikes(heap.getPrototypeOf(obj) map { from(_, heap) })


            val properties = heap.getProperties(obj, numbersOnly = false).flatMap {
                case (Some(name), prop) if name != "prototype" || !obj.isInstanceOf[FunctionEntity] =>
                    val ap = prop.abstractify(heap)
                    val ty = from(ap.value, heap, objs)

                    val propTy = Property(name, ty, optional = prop.mightBeAbsent, readonly = prop.writable.mightBeFalse)
                    Some(propTy)
                case _ =>
                    // todo give out general info
                    None
            }.toSeq

            val (signature, constructor) = obj match {
                case f: FunctionEntity =>
                    val info = f.callableInfo

                    val ths = info.thisProbe
                    val thisHasWrites = ths._writes.nonEmpty

                    lazy val prototype = from(heap.getProperty(obj, "prototype").abstractify(heap).value, heap)

                    val parameter = info.argumentProbe._reads.flatMap {
                        case (name, prop) if Try(name.toInt).isSuccess =>
                            Seq(name.toInt -> prop)
                        case _ =>
                            Seq.empty
                    }.toSeq.sortBy(_._1).map { case (i, ty) => Param(info.argumentNames(i), ty, optional = false) }

                    val returnType = from(info.returnValue, heap)

                    def overl(r: Type) = Seq(Overload(Seq.empty, parameter, r))

                    if (returnType == UndefinedType && thisHasWrites) {
                        (Seq.empty, overl(IntersectionType(new ProbeType(ths), prototype)))
                    } else {
                        (overl(returnType), Seq.empty)
                    }
                case _ =>
                    (Seq.empty, Seq.empty)
            }

            cty._resolve(bases = Seq.empty, typeParameter = Seq.empty, signature, constructor, properties)
            cty
    }




    case class Prelude(global: CompoundType, modules: Map[String, CompoundType])

    object Prelude {
        def load(prelude: Js.Obj): Prelude = {
            val types = mutable.Map.empty[Long, Type]
            val generics = mutable.Map.empty[Long, GenericType]
            val resolvers = mutable.Buffer.empty[() => Unit]
            var resolving = false

            def resolve(toResolve: () => Unit): Unit = {
                if (resolving) {
                    toResolve()
                } else {
                    resolvers += toResolve
                }
            }

            def getType(ty: Js.Value): Type = {
                assert(resolving)
                resolveType(ty) match {
                    case Left(resolved) => resolved
                    case Right(id) => types.getOrElse(id, throw new IllegalArgumentException(s"Couldn't find type for id $id"))
                }
            }

            def getTypeParameter(param: Js.Value): GenericType = {
                val generic = param.obj
                val id = generic("id").num.toLong
                val result = new GenericType
                for(constraint <- generic.get("constraint")) resolve(() => {
                    result.constraint = Some(getType(constraint))
                })
                generics += id -> result
                result
            }

            def getCallSignature(sigs: Js.Arr): Signature = {
                for (sigVal <- sigs.value; sig = sigVal.obj) yield {
                    val typeParameter = sig("typeParameter").arr map { getTypeParameter }

                    val parameters = sig("parameters").arr map {
                        param =>
                            val p = new Param(
                                name = param("name").str,
                                optional = param("optional").bool
                            )
                            val ty = param("type")
                            resolve(() =>{
                                p.ty = getType(ty)
                            })
                            p
                    }
                    val overload = new Overload(typeParameter, parameters)
                    val returnType = sig("returnType")
                    resolve(() => {
                        overload.returnType = getType(returnType)
                    })
                    overload
                }
            }

            def resolveType(ty: Js.Value, name: Option[String] = None): Either[Type, Long] = ty match {
                case Js.Num(id) =>
                    Right(id.toLong)

                case Js.Obj(properties) =>
                    def value = properties("value")

                    Left(properties("type").str match {
                        case "any" => AnyType
                        case "boolean" => BooleanType
                        case "number" => NumberType
                        case "number-lit" => NumberType
                        case "string" => StringType
                        case "string-lit" => LiteralType(value.str)
                        case "undefined" => UndefinedType
                        case "null" => NullType
                        case "void" => VoidType
                        case "never" => NeverType
                        case "symbol" => AnyType // todo: add symbol
                        case "object" => ObjectType
                        case "this" => ThisType
                        case "tuple" =>
                            val member = properties("member").arr
                            val tuple = new TupleType
                            resolve(() => {
                                tuple.members = member map { getType }
                            })
                            tuple
                        case "union" =>
                            val types = properties("types").arr
                            val union = new UnionType
                            resolve(() => {
                                union.types = types.map{ getType }.toSet
                            })
                            union

                        case "intersection" =>
                            // TODO: support intersection type
                            AnyType

                        case "index" =>
                            // TODO: support index type
                            StringType

                        case "index-access" =>
                            // TODO: support index access
                            AnyType

                        case "interface" =>
                            val isClass = properties("isClass").bool
                            val basesArr = properties("bases").arr
                            val typeParameter = properties("typeParameters").arr map { getTypeParameter }
                            val callSignatures = getCallSignature(properties("callSignatures").arr)
                            val constructor = getCallSignature(properties("constructionSignatures").arr)
                            val propertiesObj = properties("properties").obj
                            val intf = new CompoundType(name, isClass)
                            resolve(() => {
                                val bases = basesArr map { getType } map { _.asInstanceOf[ClassLike] }
                                val properties = propertiesObj.map{
                                    case (pname, prop) =>
                                        Property(
                                            name = pname,
                                            ty = getType(prop("type")),
                                            optional = prop("optional").bool,
                                            readonly = prop("readonly").bool
                                        )
                                }.toSeq
                                intf._resolve(bases, typeParameter, callSignatures, constructor, properties)
                            })
                            intf

                        case "generics-ref" =>
                            val id = properties("targetId").num.toLong
                                generics.getOrElse(id, throw new IllegalArgumentException(s"Failed to find generic id $id"))

                        case "ref" =>
                            val target = properties("targetId").num.toLong
                            val result = new Instantiate
                            val typeArguments = properties("typeParameter").arr
                            resolve(() => {
                                result.typeArguments = typeArguments map { getType }
                                result.target = types(target).asInstanceOf[CompoundType]
                            })
                            result
                    })
                case _ =>
                    throw new IllegalArgumentException("Expected type id or type definition")
            }

            def readType(ty: Js.Value, name: Option[String] = None): Type = {
                resolveType(ty, name) match {
                    case Left(resolved) => resolved
                    case Right(id) => AnyType//throw new IllegalStateException(s"Alias types are not allowed on main level! (id: $id)")
                }
            }

            for (tyValue <- prelude("types").arr; ty = tyValue.obj) {
                val id = ty("id").num.toLong
                val name = ty.get("name").map(_.str)
                types += id -> readType(ty("type"), name)
            }

            // run resolvers
            resolving = true
            resolvers foreach {
                _()
            }

            val global = getType(prelude("globalType"))
            val modules = prelude("ambientModules").arr map { module => module("name").str -> getType(module("type")) } collect { case (name, c: CompoundType) => name -> c }
            Prelude(global.asInstanceOf[CompoundType], modules.toMap)
        }
    }
}
