package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.{BoolLattice, GeneralBoolLattice}

import scala.collection.mutable
/*
trait ValueChangeHandler {
    def onValueChanged()
}
*/

abstract class ValueLike {
    def asBool: BoolLattice
    def asFunctions: Traversable[FunctionValue]
    def asObject: Option[ObjectValue]
    def baseObjects: Traversable[ObjectValue]
    def propertyWriteMaybeNoOp: Boolean

    def throwsWhenWrittenOrReadOn: Boolean
}

sealed abstract class Value extends ValueLike {

    override def asFunctions: Traversable[FunctionValue] = Seq.empty
    override def throwsWhenWrittenOrReadOn: Boolean = false
    override def asObject: Option[ObjectValue] = None
    override def propertyWriteMaybeNoOp: Boolean = true
    override def baseObjects: Traversable[ObjectValue] = Traversable()
    override def asBool: BoolLattice = BoolLattice.True
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
        case null => NullValue
        case _ => throw new IllegalArgumentException(s"Can not convert $any to a property value!")
    }
}

object NeverValue extends Value {
    override def throwsWhenWrittenOrReadOn: Boolean = true
    override def asBool: BoolLattice = BoolLattice.Top

    override def toString: String = "never"
}

object UndefinedValue extends Value {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true

    override def toString: String = "undefined"
}

object NullValue extends Value {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true

    override def toString: String = "null"
}

sealed abstract class BoolValue extends Value
object BoolValue extends BoolValue {
    override def asBool: BoolLattice = BoolLattice.Top

    override def toString: String = "boolean"
}

case class SpecificBoolValue(value: Boolean) extends BoolValue {
    override def asBool: BoolLattice = BoolLattice(value)

    override def toString: String = value.toString
}

sealed abstract class NumberValue extends Value
object NumberValue extends NumberValue {
    override def asBool: BoolLattice = BoolLattice.Top

    override def toString: String = "number"
}

case class SpecificNumberValue(value: Int) extends NumberValue {
    override def asBool: BoolLattice = BoolLattice(value != 0)

    override def toString: String = value.toString
}

sealed abstract class StringValue extends Value
object StringValue extends StringValue {
    override def asBool: BoolLattice = BoolLattice.Top

    override def toString: String = "string"
}

case class SpecificStringValue(value: String) extends StringValue {
    override def asBool: BoolLattice = BoolLattice(value != "")

    override def toString: String = "\"" + value + "\""
}

sealed class ObjectValue extends Value {
    val internalId: Long = ObjectValue.nextObjId()
    override def asObject: Option[ObjectValue] = Some(this)
    override def propertyWriteMaybeNoOp: Boolean = false

    override def toString: String = s"obj#$internalId"
}

object ObjectValue {
    private var nextId: Long = 0
    def nextObjId(): Long = {
        nextId += 1
        return nextId
    }
}

final class FunctionValue(val template: Templates.Function, val closures: Seq[Value]) extends ObjectValue() {
    override def asFunctions: Traversable[FunctionValue] = Traversable(this)

    override def toString: String = s"func#$internalId"
}

object FunctionValue {
    def unapply(fv: FunctionValue): Option[(Templates.Function, Seq[Value])] = Some((fv.template, fv.closures))
}

final class UnionValue(val values: Seq[Value]) extends ObjectValue() {

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

    override def toString: String = s"#$internalId[${values.mkString(" | ")}]"
}

object UnionValue {
    def apply(values: Value*): Value = {
        values match {
            case Seq(value) => return value
            case _ =>
        }
        var boolValue: BoolValue = null
        var numberValue: NumberValue = null
        var stringValues = mutable.Set.empty[StringValue]
        val objectValues = mutable.Set.empty[Value]
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
        }

        val seq =
            (if (hasUndefined) Seq(UndefinedValue) else Seq()) ++
            (if (hasNull) Seq(NullValue) else Seq()) ++
            Option(boolValue).toSeq ++
            Option(numberValue).toSeq ++
            Option(stringValues).getOrElse(Seq(StringValue)) ++
            objectValues

        return seq match {
            case Seq() => NeverValue
            case Seq(value) => value
            case all => new UnionValue(all)
        }
    }

    def withUndefined(value: Value): Value = value match {
        case UnionValue(values) => values match {
            case UndefinedValue +: _ => value
            case seq => new UnionValue(UndefinedValue +: seq)
        }
        case UndefinedValue => UndefinedValue
        case NeverValue => UndefinedValue
        case _ => new UnionValue(Seq(UndefinedValue, value))
    }

    def unapply(arg: UnionValue): Option[Seq[Value]] = Some(arg.values)

    private def unpackUnion(value: Value): Seq[Value] = value match {
        case UnionValue(values) => values
        case NeverValue => Seq()
        case noUnion => Seq(noUnion)
    }
}

case class UnionSet(values: Any*) {
    assert(values.size >= 2)

    override def equals(obj: scala.Any): Boolean = obj match {
        case UnionValue(seq) => seq.toSet == values.map(Value(_)).toSet
        case _ => false
    }

    override def toString: String = values.mkString("[", " | ", "]")
}

/*
class UnionValueBuilder(implicit flowAnalysis: FlowAnalysis) {
    private lazy val union = new UnionValue
    def build(values: Seq[Value]): Value = values match {
        case Seq(value) =>
            return value
        case _ =>
            values.foreach(_.flowsTo(union))
            return union
    }
}

class PropertyValue(property: String)(implicit flowAnalysis: FlowAnalysis) extends UnionValue {

    var value: HeapState.ValueHandle = _

    override def getProperty(name: String): collection.Set[HeapState.ValueHandle] = {
        if (name == property)
            Set(value)
        else
            super.getProperty(name)
    }
}*/