package inferium.js.types

import java.security.Signature

import inferium.Unifiable
import inferium.dataflow.calls.SignatureCall
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice._
import inferium.utils.Utils
import ujson.Js

import scala.collection.mutable

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

    class Instantiator(locations: Stream[Location], val thisEntity: Entity, val instantiated: mutable.Map[(Type, InstType.Type, Seq[Entity]), ObjectLike]) {

        val locs: Iterator[Location] = locations.iterator

        def newLoc(): Location = locs.next()
    }


    sealed abstract class Type extends Unifiable[Type] {
        def matches(arg: Entity): Boolean
        def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity

        override def unify(other: Type)(implicit fixpoint: Unifiable.Fixpoint): Type = Type.unify(this, other)
        override def unify(others: Seq[Type])(implicit fixpoint: Unifiable.Fixpoint): Type = Type.unify(this +: others)
    }

    object Type {
        def unify(ty: Type, rest: Type*): Type = unify(ty +: rest)
        def unify(types: Seq[Type]): Type = types match {
            case Seq() => NeverType
            case Seq(ty) => ty
            case _ => unifyRest(types)
        }

        private def unifyRest(types: Seq[Type]): Type = ???
    }

    case object AnyType extends Type {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity = AnyEntity

        override def matches(arg: Entity): Boolean = true
    }

    sealed abstract class Primitive extends Type

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

    case object ThisType extends Type {
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
            val array = heap.allocArray(arrayLoc).withAbstractCount(-1)
            ArrayUtils.fillAbstractArray(array, members.map { _.instantiate(heap, instantiator, substitutions) }, None, heap)
            array
        }

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[ObjectLike]
    }

    case object ObjectType extends Type {
        override def instantiate(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Entity= {
            AnyEntity
        }

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[ObjectLike]
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
        def apply(types: Type*): UnionType = apply(types.toSet)
        def apply(types: TraversableOnce[Type]): UnionType = {
            val res = new UnionType
            res.types = types.flatMap {
                case u: UnionType => u.types
                case any => Seq(any)
            }.toSet
            res
        }
    }

    val VoidType: UnionType = UnionType(UndefinedType, NullType)

    trait ClassLike extends Type {
        def ownProperties: Map[String, Property]
        def allProperties: Map[String, Property]
        def instProperties: Map[String, Property]

        def instantiatePrototype(heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): ObjectLike
    }

    final class Instantiate extends Type with ClassLike {
        // todo: check if typeArguments has the same length as the target's typeParameters
        var typeArguments: Seq[Type] = _
        var target: CompoundType = _

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
    }

    final class CompoundType(val name: Option[String], val isClass: Boolean) extends Type with ClassLike {
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


        private def addProperties(obj: ObjectLike, heap: Heap.Mutator, instantiator: Instantiator, substitutions: Map[GenericType, Entity]): Unit = {

            instProperties foreach {
                case (name, prop) =>
                    val value = prop.ty.instantiate(heap, instantiator, substitutions)
                    // todo: writable?
                    heap.setProperty(obj, name, AbstractProperty.defaultWriteToObject(value, prop.optional))
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

            val basePrototype = baseClass match {
                case Some(base) =>
                    base.instantiatePrototype(heap, instantiator, substitutions)
                case _ =>
                    heap.specialObject(SpecialObjects.Object)
            }

            val prototype = heap.allocOrdinaryObject(prototypeLoc, basePrototype).withAbstractCount(-1)
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
            val prorotype = if (isClass) {
                instantiatePrototype(heap, instantiator, substitutions)
            } else if (isFunctionEntity) {
                heap.specialObject(SpecialObjects.Function)
            } else {
                heap.specialObject(SpecialObjects.Object)
            }

            val paragon = heap.allocObject(paragonLoc, (loc, ac) => {
                if (isFunctionEntity) {
                    val callableInfo = SignatureCall.createCallableInfo(name, signature, constructor)
                    new SignatureFunctionEntity(loc, callableInfo)
                } else
                    OrdinaryObjectEntity(loc)(ac)
            }, prorotype).withAbstractCount(-1)
            instantiator.instantiated += instKey -> paragon

            if (!isClass) {
                addProperties(paragon, heap, instantiator, substitutions)
            }


            paragon
        }

        //override def toString: String = s"${name.getOrElse("")}{${properties.mkString(", ")}}"
        override def matches(arg: Entity): Boolean = true
    }

    /*object FunctionType {
        def apply(name: String, signature: => Signature): CompoundType = new CompoundType(Option(name), signature, Seq.empty, None)
        def apply(signature: => Signature): CompoundType = FunctionType(null, signature)
    }

    object InterfaceType {
        def apply(properties: => Seq[Property], name: String = null, extend: Type = null): CompoundType = new CompoundType(Some(name), Seq.empty, properties, Option(extend))
    }*/

    final case class Property(name: String, ty: Type, optional: Boolean = false, readonly: Boolean = false)
    type Signature = Seq[Overload]

    final class Overload(val generics: Seq[GenericType], val params: Seq[Param]) {
        var returnType: Type = _
    }
    class Param(val name: String, val optional: Boolean) {
        var ty: Type = _
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
            val modules = prelude("ambientModules").arr map { module => module("name").str -> getType(module("type")).asInstanceOf[CompoundType] }
            Prelude(global.asInstanceOf[CompoundType], modules.toMap)
        }
    }
}
