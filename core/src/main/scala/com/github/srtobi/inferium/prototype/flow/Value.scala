package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.collection.mutable
/*
trait ValueChangeHandler {
    def onValueChanged()
}
*/

abstract class ValueLike {
    def asBool: BoolLattice
    def asFunctions: Traversable[FunctionValue]
    def asObjects: Traversable[ObjectValue]

    def throwsWhenWrittenOrReadOn: Boolean
}

sealed abstract class Value extends ValueLike {

    override def asFunctions: Traversable[FunctionValue] = Seq.empty
    override def throwsWhenWrittenOrReadOn: Boolean = false
    override def asObjects: Traversable[ObjectValue] = Traversable()
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

case object NeverValue extends Value {
    override def throwsWhenWrittenOrReadOn: Boolean = true
    override def asBool: BoolLattice = BoolLattice.Top

    override def toString: String = "never"
}

case object UndefinedValue extends Value {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true

    override def toString: String = "undefined"
}

case object NullValue extends Value {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true

    override def toString: String = "null"
}

case class BoolValue(value: Boolean) extends Value {
    override def asBool: BoolLattice = BoolLattice(value)

    override def toString: String = value.toString
}
case class NumberValue(value: Int) extends Value {
    override def asBool: BoolLattice = BoolLattice(value != 0)

    override def toString: String = value.toString
}
case class StringValue(value: String) extends Value {
    override def asBool: BoolLattice = BoolLattice(value != "")

    override def toString: String = "\"" + value + "\""
}
class ObjectValue extends Value {
    val internalId: Long = ObjectValue.nextObjId()
    override def asObjects: Traversable[ObjectValue] = Traversable(this)

    override def toString: String = s"obj#$internalId"
}

object ObjectValue {
    private var nextId: Long = 0
    def nextObjId(): Long = {
        nextId += 1
        return nextId
    }
}

class FunctionValue(val template: Templates.Function, val closures: Seq[Value]) extends ObjectValue() {
    override def asFunctions: Traversable[FunctionValue] = Traversable(this)

    override def toString: String = s"func#$internalId"
}

object FunctionValue {
    def unapply(fv: FunctionValue): Option[(Templates.Function, Seq[Value])] = Some((fv.template, fv.closures))
}

class UnionValue(val values: Seq[Value]) extends ObjectValue() {

    assert(values.size >= 2)
    assert(values.forall(v => !v.isInstanceOf[UnionValue]))

    override def asBool: BoolLattice = {
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

    override def asObjects: Traversable[ObjectValue] = values.flatMap(_.asObjects) :+ this
    override def asFunctions: Traversable[FunctionValue] = values.flatMap(_.asFunctions)
    override def throwsWhenWrittenOrReadOn: Boolean = values.forall(_.throwsWhenWrittenOrReadOn)

    override def toString: String = values.mkString(" | ")
}

object UnionValue {
    def apply(values: Value*): Value = {
        return values.flatMap(unpackUnion).distinct match {
            case Seq() => NeverValue
            case Seq(value) => value
            case seq => new UnionValue(seq)
        }
    }

    def unapply(arg: UnionValue): Option[Seq[Value]] = Some(arg.values)

    private def unpackUnion(value: Value): Seq[Value] = value match {
        case UnionValue(values) => values
        case NeverValue => Seq()
        case noUnion => Seq(noUnion)
    }
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