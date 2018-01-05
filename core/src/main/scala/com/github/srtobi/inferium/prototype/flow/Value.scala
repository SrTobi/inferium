package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.collection.mutable

abstract class ValueLike {
    def asValue: Value
    def asBool: BoolLattice
    def asFunctions: Traversable[FunctionValue]
    def asObject: Option[ObjectValue]
    def baseObjects: Traversable[ObjectValue]
    def propertyWriteMaybeNoOp: Boolean

    def without(filter: (ValueLike) => Boolean): ValueLike

    def truthy(heap: HeapMemory): ValueLike
    def falsy(heap: HeapMemory): ValueLike

    def throwsWhenWrittenOrReadOn: Boolean

    def asReference: Option[Reference]
}

sealed abstract class Value extends ValueLike {

    override def asValue: Value = this
    override def asBool: BoolLattice = BoolLattice.True
    override def asFunctions: Traversable[FunctionValue] = Seq.empty
    override def asObject: Option[ObjectValue] = None
    override def propertyWriteMaybeNoOp: Boolean = true
    override def baseObjects: Traversable[ObjectValue] = Traversable()

    override def without(filter: ValueLike => Boolean): ValueLike = if (filter(this)) NeverValue else this

    override def throwsWhenWrittenOrReadOn: Boolean = false

    def asReference: Option[Reference] = None

    /*def in: scala.collection.Set[Value] = inValues
    def out: scala.collection.Set[Value] = outValues

    final def flowsTo(toValue: Value): Unit = {
        if (outValues.add(toValue)) {
            toValue.inValues += this

            // notify
            this.onOutFlow(toValue)
            toValue.onInFlow(this)
        }
    }

    final def removeFlowTo(toValue: Value): Unit = {
        if (outValues.remove(toValue)) {
            toValue.inValues -= this

            // notify
            this.onOutFlowRemoved(toValue)
            toValue.onInFlowRemoved(this)
        }
    }

    def onInFlow(inValue: Value): Unit = {}
    def onOutFlow(outValue: Value): Unit = {}
    def onInFlowRemoved(inValue: Value): Unit = {}
    def onOutFlowRemoved(outValue: Value): Unit = {}*/

    //def getProperty(name: String): scala.collection.Set[HeapState.ValueHandle]
}

object Value {
    def apply(any: Any): Value = any match {
        case value: Value => value
        case number: Int => SpecificNumberValue(number)
        case string: String => SpecificStringValue(string)
        case bool: Boolean => SpecificBoolValue(bool)
        case null => NullValue
        case _ => throw new IllegalArgumentException(s"Can not convert $any to a property value!")
    }
}

object NeverValue extends Value {
    override def throwsWhenWrittenOrReadOn: Boolean = true
    override def asBool: BoolLattice = BoolLattice.Top

    override def truthy(heap: HeapMemory): ValueLike = NeverValue
    override def falsy(heap: HeapMemory): ValueLike = NeverValue

    override def toString: String = "never"
}

object UndefinedValue extends Value {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true

    override def truthy(heap: HeapMemory): ValueLike = NeverValue
    override def falsy(heap: HeapMemory): ValueLike = this

    override def toString: String = "undefined"
}

object NullValue extends Value {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true

    override def truthy(heap: HeapMemory): ValueLike = NeverValue
    override def falsy(heap: HeapMemory): ValueLike = this

    override def toString: String = "null"
}

sealed abstract class BoolValue extends Value {
    override def truthy(heap: HeapMemory): ValueLike = SpecificBoolValue(true)
    override def falsy(heap: HeapMemory): ValueLike = SpecificBoolValue(false)
}
object BoolValue extends BoolValue {
    override def asBool: BoolLattice = BoolLattice.Top

    override def toString: String = "boolean"
}

sealed abstract class SpecificBoolValue protected(val value: Boolean) extends BoolValue {
    override def toString: String = value.toString
}

object TrueValue extends SpecificBoolValue(true) {
    override def asBool: BoolLattice = BoolLattice.True

}

object FalseValue extends SpecificBoolValue(false) {
    override def asBool: BoolLattice = BoolLattice.False

    override def toString: String = "false"
}

object SpecificBoolValue {
    def apply(value: Boolean): SpecificBoolValue = if (value) TrueValue else FalseValue

    def unapply(arg: SpecificBoolValue): Option[Boolean] = Some(arg.value)
}

sealed abstract class NumberValue extends Value
object NumberValue extends NumberValue {
    override def asBool: BoolLattice = BoolLattice.Top

    override def truthy(heap: HeapMemory): ValueLike = this
    override def falsy(heap: HeapMemory): ValueLike = SpecificNumberValue(0)

    override def toString: String = "number"
}

case class SpecificNumberValue(value: Int) extends NumberValue {
    override def asBool: BoolLattice = BoolLattice(value != 0)

    override def truthy(heap: HeapMemory): ValueLike = if (value == 0) NeverValue else this
    override def falsy(heap: HeapMemory): ValueLike = if (value == 0) this else NeverValue

    override def toString: String = value.toString
}

sealed abstract class StringValue extends Value
object StringValue extends StringValue {
    override def asBool: BoolLattice = BoolLattice.Top

    override def truthy(heap: HeapMemory): ValueLike = this
    override def falsy(heap: HeapMemory): ValueLike = SpecificStringValue("")

    override def toString: String = "string"
}

case class SpecificStringValue(value: String) extends StringValue {
    override def asBool: BoolLattice = BoolLattice(value != "")

    override def truthy(heap: HeapMemory): ValueLike = if (value == "") NeverValue else this
    override def falsy(heap: HeapMemory): ValueLike = if (value == "") this else NeverValue

    override def toString: String = "\"" + value + "\""
}

sealed class ObjectValue(val internalId: Long = ObjectValue.nextObjId()) extends Value {
    override def asObject: Option[ObjectValue] = Some(this)
    override def propertyWriteMaybeNoOp: Boolean = false

    override def truthy(heap: HeapMemory): ValueLike = this
    override def falsy(heap: HeapMemory): ValueLike = NeverValue

    override def toString: String = s"obj#$internalId"
}

object ObjectValue {
    private var nextId: Long = 0
    def nextObjId(): Long = {
        nextId += 1
        return nextId
    }
}

final class FunctionValue(val template: Templates.Function, val closures: Seq[ValueLike]) extends ObjectValue() {
    override def asFunctions: Traversable[FunctionValue] = Traversable(this)

    override def toString: String = s"func#$internalId"
}

object FunctionValue {
    def unapply(fv: FunctionValue): Option[(Templates.Function, Seq[ValueLike])] = Some((fv.template, fv.closures))
}

final class UnionValue private (id: Long, val values: Seq[ValueLike]) extends ObjectValue(id) {

    assert(values.size >= 2)
    assert(values.forall(v => !v.isInstanceOf[UnionValue]))

    override lazy val asBool: BoolLattice = asBoolImpl()
    private def asBoolImpl(): BoolLattice = {
        val it = values.iterator
        var result: BoolLattice = it.next().asBool
        for (bool <- it) {
            result = result.unify(bool.asBool)
            if (result == BoolLattice.Top) {
                return BoolLattice.Top
            }
        }
        return result
    }

    override def asObject: Option[ObjectValue] = Some(this)
    override def baseObjects: Traversable[ObjectValue] = values.flatMap(_.asObject)
    override def propertyWriteMaybeNoOp: Boolean = values.exists(_.propertyWriteMaybeNoOp)
    override def asFunctions: Traversable[FunctionValue] = values.flatMap(_.asFunctions)
    override def throwsWhenWrittenOrReadOn: Boolean = values.forall(_.throwsWhenWrittenOrReadOn)

    override def without(filter: ValueLike => Boolean): ValueLike = {
        val newSeq = values.filter(!filter(_))
        return if (newSeq.length == values.length) this else UnionValue.unionFromSeq(internalId, newSeq)
    }

    override def truthy(heap: HeapMemory): ValueLike = UnionValue.makeUnion(internalId, values.map(_.truthy(heap)))
    override def falsy(heap: HeapMemory): ValueLike = UnionValue.makeUnion(internalId, values.map(_.falsy(heap)))

    override def toString: String = s"#$internalId[${values.mkString(" | ")}]"
}

object UnionValue {
    private def unionFromSeq(id: Long, values: Seq[ValueLike]): ValueLike = {
        return values match {
            case Seq() => NeverValue
            case Seq(value) => value
            case _ => new UnionValue(id, values)
        }
    }

    def apply(values: ValueLike*): ValueLike = {
        return values match {
            case Seq(value) => value
            case Seq(u1:UnionValue, u2:UnionValue) if u1.internalId == u2.internalId => makeUnion(u1.internalId, values)
            case all => makeUnion(ObjectValue.nextObjId(), all)
        }
    }


    private def makeUnion(id: Long, values: Seq[ValueLike]): ValueLike = {
        values match {
            case Seq(value) => return value
            case _ =>
        }
        var boolValue: BoolValue = null
        var numberValue: NumberValue = null
        var stringValues = mutable.Set.empty[StringValue]
        val objectValues = mutable.Set.empty[ValueLike]
        val conditionalValues = mutable.Set.empty[ConditionalValue]
        var hasUndefined = false
        var hasNull = false

        values.flatMap(unpackUnion) foreach {
            case bool: BoolValue =>
                if (boolValue == null || boolValue == bool)
                    boolValue = bool
                else
                    boolValue = BoolValue
            case number: NumberValue =>
                if (numberValue == null || numberValue == number)
                    numberValue = number
                else
                    numberValue = NumberValue
            case StringValue =>
                stringValues = null
            case string: SpecificStringValue =>
                if (stringValues ne null)
                    stringValues.add(string)
            case UndefinedValue =>
                hasUndefined = true
            case NullValue =>
                hasNull = true
            case obj: ObjectValue =>
                objectValues.add(obj)
            case NeverValue =>
            case value: ConditionalValue =>
                conditionalValues.add(value)
        }

        return unionFromSeq(id,
            (if (hasUndefined) Seq(UndefinedValue) else Seq()) ++
            (if (hasNull) Seq(NullValue) else Seq()) ++
            Option(boolValue).toSeq ++
            Option(numberValue).toSeq ++
            Option(stringValues).getOrElse(Seq(StringValue)) ++
            objectValues ++
            conditionalValues)
    }

    def withUndefined(value: ValueLike): ValueLike = value match {
        case u@UnionValue(values) => values match {
            case UndefinedValue +: _ => value
            case seq => new UnionValue(u.internalId, UndefinedValue +: seq)
        }
        case UndefinedValue => UndefinedValue
        case NeverValue => UndefinedValue
        case _ => UnionValue(UndefinedValue, value)
    }

    def unapply(arg: UnionValue): Option[Seq[ValueLike]] = Some(arg.values)

    private def unpackUnion(value: ValueLike): Seq[ValueLike] = value match {
        case UnionValue(values) => values
        case NeverValue => Seq()
        case noUnion => Seq(noUnion)
    }
}

case class UnionSet(values: Any*) {
    assert(values.size >= 2)

    override def equals(obj: scala.Any): Boolean = obj match {
        case UnionValue(seq) => seq.map(_.asValue).toSet == values.map(Value(_)).toSet
        case _ => false
    }

    override def toString: String = values.mkString("[", " | ", "]")
}

sealed abstract class ConditionalValue extends ValueLike {
    override def asBool: BoolLattice = asValue.asBool
    override def asFunctions: Traversable[FunctionValue] = asValue.asFunctions
    override def asObject: Option[ObjectValue] = asValue.asObject
    override def baseObjects: Traversable[ObjectValue] = asValue.baseObjects
    override def propertyWriteMaybeNoOp: Boolean = asValue.propertyWriteMaybeNoOp
    override def throwsWhenWrittenOrReadOn: Boolean = asValue.throwsWhenWrittenOrReadOn

    override def without(filter: ValueLike => Boolean): ValueLike = asValue.without(filter)
}

final case class Reference(value: ValueLike, baseObject: ValueLike, property: String) extends ConditionalValue {
    override val asValue: Value = value.asValue

    def asReference: Option[Reference] = Some(this)

    override def truthy(heap: HeapMemory): ValueLike = {
        heap.manipulateReference(baseObject, _.without(heap.readProperty(_, property).asBool == BoolLattice.False))
        return value.truthy(heap)
    }
    override def falsy(heap: HeapMemory): ValueLike = {
        heap.manipulateReference(baseObject, _.without(heap.readProperty(_, property).asBool == BoolLattice.True))
        return value.falsy(heap)
    }

    override def toString: String = s"$value{$baseObject.$property}"
}
