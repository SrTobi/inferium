package inferium.js.types

import java.security.Signature

import inferium.Unifiable
import inferium.dataflow.calls.SignatureCall
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice._
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


    sealed abstract class Type extends Unifiable[Type] {
        def matches(arg: Entity): Boolean
        def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity

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
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = AnyEntity

        override def matches(arg: Entity): Boolean = true
    }

    sealed abstract class Primitive extends Type

    case object NeverType extends Primitive {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = NeverValue

        override def matches(arg: Entity): Boolean = false
    }
    case object UndefinedType extends Primitive {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = UndefinedValue

        override def matches(arg: Entity): Boolean = arg == UndefinedValue
    }
    case object NullType extends Primitive {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = NullValue

        override def matches(arg: Entity): Boolean = arg == NullValue
    }

    sealed abstract class BooleanType extends Primitive
    case object BooleanType extends BooleanType {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = BoolValue

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[BoolValue]
    }

    case object TrueType extends BooleanType {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = TrueValue

        override def matches(arg: Entity): Boolean = arg == TrueValue
    }
    case object FalseType extends BooleanType {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = FalseValue

        override def matches(arg: Entity): Boolean = arg == FalseValue
    }

    case object NumberType extends Primitive {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = NumberValue

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[NumberValue]
    }

    sealed abstract class StringType extends Primitive
    case object StringType extends StringType {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = StringValue

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[StringValue]
    }

    final case class LiteralType(value: String) extends StringType {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = SpecificStringValue(value)

        override def matches(arg: Entity): Boolean = arg match {
            case SpecificStringValue(otherValue) => this.value == otherValue
            case _ => false
        }
    }

    case object ThisType extends Type {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = {
            assert(thisEntity != NeverValue)
            thisEntity
        }

        override def matches(arg: Entity): Boolean = false
    }

    final class TupleType extends Type {
        var members: Seq[Type] = _

        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = {
            val arrayLoc = locGen.loc()
            val array = heap.allocArray(arrayLoc).withAbstractCount(-1)
            ArrayUtils.fillAbstractArray(array, members.map { _.instantiate(locGen, heap, substitutions, thisEntity, objs) }, None, heap)
            array
        }

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[ObjectLike]
    }

    case object ObjectType extends Type {
        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = {
            AnyEntity
        }

        override def matches(arg: Entity): Boolean = arg.isInstanceOf[ObjectLike]
    }

    final class GenericType(/*val name: String, */) extends Type {
        var constraint: Option[Type] = None

        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = {
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

        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = {
            Entity.unify(types map { _.instantiate(locGen, heap, substitutions, thisEntity, objs) })
        }

        override def matches(arg: Entity): Boolean = true
    }

    object UnionType {
        def apply(types: Type*): UnionType = apply(types.toSet)
        def apply(types: Set[Type]): UnionType = {
            val res = new UnionType
            res.types = types
            res
        }
    }

    val VoidType: UnionType = UnionType(UndefinedType, NullType)

    final class Instantiate extends Type {
        // todo: check if typeArguments has the same length as the target's typeParameters
        var typeArguments: Seq[Type] = _
        var target: CompoundType = _

        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = {
            assert(typeArguments.length == target.typeParameter.length)

            // todo: respect default type parameter
            val newSubstitution = (target.typeParameter zip typeArguments).map {
                case (generic, ty) => (generic, ty.instantiate(locGen, heap, substitutions, thisEntity, objs))
            }.toMap

            target.instantiate(locGen, heap, newSubstitution, thisEntity, objs, this)
        }

        override def matches(arg: Entity): Boolean = true
    }

    final class CompoundType(val name: Option[String]) extends Type {
        var bases: Seq[Type] = _
        var typeParameter: Seq[GenericType] = _
        var signature: Signature = _
        var constructor: Signature = _

        var properties: Map[String, Property] = _

        def callable: Boolean = signature.nonEmpty
        def constructable: Boolean = constructor.nonEmpty

        private[js] def _resolve(bases: Seq[Type],
                                 typeParameter: Seq[GenericType],
                                 signature: Signature,
                                 constructor: Signature,
                                 properties: Seq[Property]): Unit = {
            this.bases = bases
            this.typeParameter = typeParameter
            this.signature = signature
            this.constructor = constructor
            this.properties = properties.map(p => p.name -> p).toMap
            assert(this.properties.size == properties.size)
        }

        override def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity]): Entity = {
           instantiate(locGen, heap, substitutions, thisEntity, objs, this)
        }


        def instantiate(locGen: LocGen, heap: Heap.Mutator, substitutions: Map[GenericType, Entity], thisEntity: Entity, objs: mutable.Map[Type, Entity], inst: Type): Entity = {
            objs.get(inst) foreach {
                return _
            }

            val objLoc = locGen.loc()

            val prototype = if (bases.isEmpty) {
                heap.specialObject(SpecialObjects.Function)
            } else{
                Entity.unify(bases map { _.instantiate(locGen, heap, substitutions, thisEntity, objs) })
            }

            val obj = heap.allocObject(objLoc, (loc, ac) => {
                if (signature.isEmpty && constructor.isEmpty)
                    OrdinaryObjectEntity(loc)(-1)
                else
                    new SignatureFunctionEntity(loc, SignatureCall.createCallableInfo(name, signature, constructor))
            }, prototype)

            objs += inst -> obj

            properties foreach {
                case (name, prop) =>
                    val value = prop.ty.instantiate(locGen, heap, substitutions, thisEntity, objs)
                    // todo: writable?
                    heap.setProperty(obj, name, AbstractProperty.defaultWriteToObject(value, prop.optional))
            }

            obj
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


    case class Prelude(global: Type, modules: Map[String, Type])

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
                            val intf = new CompoundType(name)
                            resolve(() => {
                                val bases = basesArr map { getType }
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
            val modules = prelude("ambientModules").arr map { module => module("name").str -> getType(module("type")) }
            Prelude(global, modules.toMap)
        }
    }
}
