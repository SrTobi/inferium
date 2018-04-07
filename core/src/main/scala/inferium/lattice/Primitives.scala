package inferium.lattice

import scala.collection.mutable

sealed abstract class Primitive extends Entity


object NeverValue extends Primitive {

    override def unify(other: Entity): Entity = other

    override def toString: String = "never"
}

object UndefinedValue extends Primitive {
    override def toString: String = "undefined"
}

object NullValue extends Primitive {
    override def toString: String = "null"
}

sealed abstract class BoolValue extends Primitive {
    def toLattice: BoolLattice
}
object BoolValue extends BoolValue {
    def apply(value: BoolLattice): BoolValue = value match {
        case BoolLattice.Top => BoolValue
        case BoolLattice.False => FalseValue
        case BoolLattice.True => TrueValue
    }

    def unapply(arg: BoolValue): Option[BoolLattice] = Some(arg.toLattice)

    override def toString: String = "boolean"
    override val toLattice: BoolLattice = BoolLattice.Top
}



sealed abstract class SpecificBoolValue protected(val value: Boolean) extends BoolValue {
    override def toString: String = value.toString
    override val toLattice: BoolLattice = BoolLattice(false)
}

object TrueValue extends SpecificBoolValue(true) {

    override def toString: String = "true"
}

object FalseValue extends SpecificBoolValue(false) {

    override def toString: String = "false"
}

object SpecificBoolValue {
    def apply(value: Boolean): SpecificBoolValue = if (value) TrueValue else FalseValue

    def unapply(arg: SpecificBoolValue): Option[Boolean] = Some(arg.value)
}



sealed abstract class NumberValue extends Primitive
object NumberValue extends NumberValue {

    override def toString: String = "number"
}

case class SpecificNumberValue(value: Int) extends NumberValue {

    override def toString: String = value.toString
}



sealed abstract class StringValue extends Primitive
object StringValue extends StringValue {

    def apply(string: String): SpecificStringValue = SpecificStringValue(string)
    override def toString: String = "string"
}

class SpecificStringValue private (val value: String) extends StringValue {

    override def toString: String = "\"" + value + "\""
}

object SpecificStringValue {
    private val stringCache = mutable.HashMap.empty[String, SpecificStringValue]

    def apply(string: String): SpecificStringValue = stringCache.synchronized {
        stringCache.getOrElseUpdate(string, new SpecificStringValue(string))
    }

    def unapply(arg: SpecificStringValue): Option[String] = Some(arg.value)
}
