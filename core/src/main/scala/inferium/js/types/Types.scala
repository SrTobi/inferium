package inferium.js.types

import java.security.Signature

import inferium.Unifiable

// noinspection ScalaFileName
object js {

    sealed abstract class Type extends Unifiable[Type] {
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

    case object AnyType extends Type

    sealed abstract class Primitive extends Type

    case object NeverType extends Primitive
    case object UndefinedType extends Primitive
    case object NullType extends Primitive

    sealed abstract class BooleanType extends Primitive
    case object BooleanType extends BooleanType

    case object TrueType extends BooleanType
    case object FalseType extends BooleanType

    case object Number extends Primitive

    sealed abstract class StringType extends Primitive
    case object StringType extends StringType

    final case class LiteralType(value: String) extends StringType

    final class GenericType(val name: String, _extend: => Option[Type]) extends Type {
        lazy val extend: Option[Type] = _extend
    }

    final class CompoundType(val name: Option[String], _signature: => Signature, _propertyList: => Seq[Property], extend: Option[Type]) extends Type {
        lazy val signature: Signature = _signature

        lazy val properties: Map[String, Property] = {
            val propertyList = _propertyList
            val result = propertyList.map(p => p.name -> p).toMap
            assert(result.size == propertyList.size)
            result
        }

        def isFunction: Boolean = signature.nonEmpty
    }

    object FunctionType {
        def apply(name: String, signature: => Signature): CompoundType = new CompoundType(Option(name), signature, Seq.empty, None)
        def apply(signature: => Signature): CompoundType = FunctionType(null, signature)
    }

    object InterfaceType {
        def apply(properties: => Seq[Property], name: String = null, extend: Type = null): CompoundType = new CompoundType(Some(name), Seq.empty, properties, Option(extend))
    }

    final case class Property(name: String, ty: Type, readonly: Boolean = false)
    type Signature = Seq[Overload]

    case class Overload(params: Seq[Param])
    case class Param(name: String, ty: Type)
}
