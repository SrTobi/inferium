package inferium.lattice

import scala.collection.mutable

sealed abstract class Primitive extends Entity {
    override def isNormalized: Boolean = true
    override def normalized(heap: Heap.Mutator): Entity = this
}


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


/************************ String ************************/
sealed abstract class BoolValue extends Primitive {
    def toLattice: BoolLattice
}
object BoolValue extends BoolValue {
    def apply(value: BoolLattice): BoolValue = value match {
        case BoolLattice.Top => BoolValue
        case BoolLattice.False => FalseValue
        case BoolLattice.True => TrueValue
    }

    def apply(value: Boolean): BoolValue = apply(BoolLattice(value))

    def unapply(arg: BoolValue): Option[BoolLattice] = Some(arg.toLattice)

    override def mightBe(entity: Entity): Boolean = entity match {
        case _: BoolValue => true
        case _ => super.mightBe(entity)
    }

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



/************************ Number ************************/
sealed abstract class NumberValue extends Primitive
object NumberValue extends NumberValue {

    override def mightBe(entity: Entity): Boolean = entity match {
        case _: NumberValue => true
        case _ => super.mightBe(entity)
    }

    override def toString: String = "number"
}

case class SpecificNumberValue(value: Int) extends NumberValue {

    override def toString: String = value.toString
}


/************************ String ************************/
sealed abstract class StringValue extends Primitive

object StringValue extends StringValue {

    override def mightBe(entity: Entity): Boolean = entity match {
        case _: StringValue => true
        case _ => super.mightBe(entity)
    }

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
