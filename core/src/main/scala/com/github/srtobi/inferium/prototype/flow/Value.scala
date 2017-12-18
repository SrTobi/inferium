package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.collection.mutable

trait ValueChangeHandler {
    def onValueChanged()
}

sealed abstract class Value(implicit val flowAnalysis: FlowAnalysis) {
    protected val inValues = mutable.Set.empty[Value]
    protected val outValues = mutable.Set.empty[Value]

    def in: scala.collection.Set[Value] = inValues
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
    def onOutFlowRemoved(outValue: Value): Unit = {}

    def asBool: BoolLattice
    def asConcreteValue: Option[ConcreteValue]
    def asFunctions: Traversable[FunctionValue]

    def throwsWhenWrittenOrReadOn: Boolean
    def getProperty(name: String): scala.collection.Set[Heap.ValueHandle]
}

sealed abstract class ConcreteValue(implicit flowAnalysis: FlowAnalysis) extends Value {

    override def asConcreteValue: Option[ConcreteValue] = Some(this)
    override def asFunctions: Traversable[FunctionValue] = Seq.empty
    override def throwsWhenWrittenOrReadOn: Boolean = false

    override def asBool: BoolLattice = BoolLattice.True
    override def getProperty(name: String): collection.Set[Heap.ValueHandle] = Set()
}

case class NeverValue()(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue

case class UndefinedValue()(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true
}

case class NullValue()(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true
}

case class BoolValue(value: Boolean)(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue {
    override def asBool: BoolLattice = BoolLattice(value)
}
case class NumberValue(value: Int)(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue {
    override def asBool: BoolLattice = BoolLattice(value != 0)
}
case class StringValue(value: String)(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue {
    override def asBool: BoolLattice = BoolLattice(value != "")
}
case class EmptyObject(id: Int)(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue

case class FunctionValue(template: Templates.Function, closures: Seq[Heap.ValueHandle])(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue {
    override def asFunctions: Traversable[FunctionValue] = Seq(this)
}

class UnionValue()(implicit flowAnalysis: FlowAnalysis) extends Value {

    override def asBool: BoolLattice = {
        assert(in.nonEmpty)
        var result: BoolLattice = in.head.asBool
        for (bool <- in.iterator.drop(1)) {
            result = result.unify(bool.asBool)
            if (result == BoolLattice.Top) {
                return BoolLattice.Top
            }
        }
        return result
    }

    override def asConcreteValue: Option[ConcreteValue] = {
        assert(in.nonEmpty)
        return in.head.asConcreteValue.map {
            result =>
                for (value <- in.iterator.drop(1)) {
                    value.asConcreteValue match {
                        case Some(concrete) if concrete == result =>
                            ()
                        case _ =>
                            return None
                    }
                }
                result
        }
    }

    override def asFunctions: Traversable[FunctionValue] = in.flatMap(_.asFunctions)
    override def throwsWhenWrittenOrReadOn: Boolean = in.forall(_.throwsWhenWrittenOrReadOn)
    override def getProperty(name: String): collection.Set[Heap.ValueHandle] = in.foldLeft(Set[Heap.ValueHandle]())(_ | _.getProperty(name))
}

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

    var value: Heap.ValueHandle = _

    override def getProperty(name: String): collection.Set[Heap.ValueHandle] = {
        if (name == property)
            Set(value)
        else
            super.getProperty(name)
    }
}