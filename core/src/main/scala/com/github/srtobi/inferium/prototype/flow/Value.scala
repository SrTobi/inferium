package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.collection.mutable

sealed abstract class Value(implicit val flowAnalysis: FlowAnalysis) {
    private val inValues = mutable.Set.empty[Value]
    private val outValues = mutable.Set.empty[Value]

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

    def onInFlow(inValue: Value): Unit = {}
    def onOutFlow(outValue: Value): Unit = {}

    def asBool: BoolLattice
    def asConcreteValue: Option[ConcreteValue]
    def asFunctions: Traversable[FunctionValue]

    def throwsWhenWrittenOrReadOn: Boolean

}

sealed abstract class ConcreteValue(implicit flowAnalysis: FlowAnalysis) extends Value {

    override def asConcreteValue: Option[ConcreteValue] = Some(this)
    override def asFunctions: Traversable[FunctionValue] = Seq.empty
    override def throwsWhenWrittenOrReadOn: Boolean = false

    override def asBool: BoolLattice = BoolLattice.True
}

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

case class FunctionValue(template: Templates.Function, closures: Seq[EmptyObject])(implicit flowAnalysis: FlowAnalysis) extends ConcreteValue {
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
    override def throwsWhenWrittenOrReadOn: Boolean = in.forall(_ throwsWhenWrittenOrReadOn)
}
