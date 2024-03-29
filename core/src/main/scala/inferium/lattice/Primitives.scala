package inferium.lattice

import inferium.Unifiable
import inferium.dataflow.CallableInfo
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice.assertions.{Assertion, Falsyfied, Propertyfied, Truthyfied}
import inferium.utils.macros.blockRec

import scala.collection.mutable

sealed abstract class Primitive extends Entity {
    override def isNormalized: Boolean = true

    @blockRec(nonrec = true)
    override def normalized(heap: Heap.Mutator): Entity = this

    @blockRec
    def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive

    @blockRec(nonrec = true)
    protected[lattice] override def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Boolean, Assertion.Effect) = {
        val entity = withAssertion(assertion, heap)
        (entity, entity != this, Assertion.noEffect(entity))
    }

    @blockRec(nonrec = true)
    override def asProbes(heap: Heap.Mutator): Seq[ProbeEntity] = Seq.empty

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = Seq(heap.specialObject(SpecialObjects.Object))

    override def coerceToConstructionObject(heap: Heap.Mutator, constructionObject: ObjectLike): Seq[ObjectLike] = Seq(constructionObject)

    def coerceToCallables(heap: Heap.Mutator, fail: () => Unit): Seq[FunctionEntity] = {
        fail()
        Seq()
    }
}


object NeverValue extends Primitive {

    override def unify(other: Entity)(implicit fixpoint: Unifiable.Fixpoint): Entity = other
    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = Seq()

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): GeneralBoolLattice = GeneralBoolLattice.Bottom

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice.Bottom

    //@blockRec(nonrec = true)
    //override def withAssertion(cond: Entity => Boolean, heap: Heap.Mutator): NeverValue.type = NeverValue

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): NeverValue.type = NeverValue

    @blockRec(nonrec = true)
    override def asTypeof(heap: Heap.Mutator): Set[String] = Set.empty

    override def toString: String = "never"
}

object UndefinedValue extends Primitive {
    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = Seq()

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.False

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice("undefined")

    @blockRec(nonrec = true)
    override def asTypeof(heap: Heap.Mutator): Set[String] = Set("undefined")

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => NeverValue
        case Falsyfied => this
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def toString: String = "undefined"
}

object NullValue extends Primitive {
    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = Seq()

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.False

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice("null")

    @blockRec(nonrec = true)
    override def asTypeof(heap: Heap.Mutator): Set[String] = Set("object")

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => NeverValue
        case Falsyfied => this
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def toString: String = "null"
}


/************************ String ************************/
sealed abstract class BoolValue extends Primitive {
    def toLattice: BoolLattice

    def negate: BoolValue

    @blockRec(nonrec = true)
    override def asTypeof(heap: Heap.Mutator): Set[String] = Set("boolean")
}
object BoolValue extends BoolValue {
    def apply(value: BoolLattice): BoolValue = value match {
        case BoolLattice.Top => BoolValue
        case BoolLattice.False => FalseValue
        case BoolLattice.True => TrueValue
    }

    def apply(value: GeneralBoolLattice): Entity = value match {
        case BoolLattice.Top => BoolValue
        case BoolLattice.False => FalseValue
        case BoolLattice.True => TrueValue
        case GeneralBoolLattice.Bottom => NeverValue
    }


    def apply(value: Boolean): BoolValue = apply(BoolLattice(value))

    def unapply(arg: BoolValue): Option[BoolLattice] = Some(arg.toLattice)

    override def mightBe(entity: Entity): Boolean = entity match {
        case _: BoolValue => true
        case _ => super.mightBe(entity)
    }

    override val toLattice: BoolLattice = BoolLattice.Top

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.Top

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice("true", "false")

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => TrueValue
        case Falsyfied => FalseValue
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def negate: BoolValue = BoolValue

    override def toString: String = "boolean"
}



sealed abstract class SpecificBoolValue protected(val value: Boolean) extends BoolValue {
    override def toString: String = value.toString
    override val toLattice: BoolLattice = BoolLattice(false)
}

object TrueValue extends SpecificBoolValue(true) {

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.True

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice("true")

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => this
        case Falsyfied => NeverValue
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def toString: String = "true"

    override def negate: BoolValue = FalseValue
}

object FalseValue extends SpecificBoolValue(false) {

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.False

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice("false")

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => NeverValue
        case Falsyfied => this
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def toString: String = "false"

    override def negate: BoolValue = TrueValue
}

object SpecificBoolValue {
    def apply(value: Boolean): SpecificBoolValue = if (value) TrueValue else FalseValue

    def unapply(arg: SpecificBoolValue): Option[Boolean] = Some(arg.value)
}



/************************ Number ************************/
sealed abstract class NumberValue extends Primitive {
    @blockRec(nonrec = true)
    override def asTypeof(heap: Heap.Mutator): Set[String] = Set("number")
}
object NumberValue extends NumberValue {

    override def mightBe(entity: Entity): Boolean = entity match {
        case _: NumberValue => true
        case _ => super.mightBe(entity)
    }

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.Top

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice.NumberString

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => this
        case Falsyfied => SpecificNumberValue(0)
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def toString: String = "number"
}

case class SpecificNumberValue(value: Long) extends NumberValue {

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice(value != 0)

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice(value.toString)

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => if (value == 0) NeverValue else this
        case Falsyfied => if (value == 0) this else NeverValue
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def toString: String = value.toString
}


/************************ String ************************/
sealed abstract class StringValue extends Primitive {
    @blockRec(nonrec = true)
    override def asTypeof(heap: Heap.Mutator): Set[String] = Set("string")}

object StringValue extends StringValue {

    override def mightBe(entity: Entity): Boolean = entity match {
        case _: StringValue => true
        case _ => super.mightBe(entity)
    }

    def apply(string: String): SpecificStringValue = SpecificStringValue(string)

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.Top

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice.Top

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => this
        case Falsyfied => SpecificStringValue.emptyString
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def toString: String = "string"
}

class SpecificStringValue private (val value: String) extends StringValue {

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice(value != "")

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice(value)

    @blockRec(nonrec = true)
    override def withAssertion(assertion: Assertion, heap: Heap.Mutator): Primitive = assertion match {
        case Truthyfied => if (value == "") NeverValue else this
        case Falsyfied => if (value == "") this else NeverValue
        case Propertyfied(_, _) => NeverValue // todo: check for properties
    }

    override def toString: String = "\"" + value + "\""
}

object SpecificStringValue {
    private val stringCache = mutable.HashMap.empty[String, SpecificStringValue]

    val emptyString: SpecificStringValue = SpecificStringValue("")

    def apply(string: String): SpecificStringValue = stringCache.synchronized {
        stringCache.getOrElseUpdate(string, new SpecificStringValue(string))
    }

    def unapply(arg: SpecificStringValue): Option[String] = Some(arg.value)
}
