package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

trait Value {
    def flowsTo(upperBound: Value): Unit

    def asBool: BoolLattice
    def asConcreteValue: Option[ConcreteValue]
    def asFunctions: Traversable[FunctionValue]

    def throwsWhenWrittenOrReadOn: Boolean

}

sealed abstract class ConcreteValue extends Value {
    override def flowsTo(upperBound: Value): Unit = ???

    override def asConcreteValue: Option[ConcreteValue] = Some(this)
    override def asFunctions: Traversable[FunctionValue] = Seq.empty
    override def throwsWhenWrittenOrReadOn: Boolean = false
}

case class ConcreteBool(value: Boolean) extends ConcreteValue {
    override def asBool: BoolLattice = BoolLattice(value)
}
case class ConcreteNumber(value: Int) extends ConcreteValue {
    override def asBool: BoolLattice = BoolLattice(value != 0)
}
case class ConcreteString(value: String) extends ConcreteValue {
    override def asBool: BoolLattice = BoolLattice(value != "")
}

case class FunctionValue(template: Templates.Function, closures: Seq[Value]) extends Value {
    override def flowsTo(upperBound: Value): Unit = ???

    override def asBool: BoolLattice = BoolLattice.True

    override def asConcreteValue: Option[ConcreteValue] = None
    override def asFunctions: Traversable[FunctionValue] = Seq(this)
    override def throwsWhenWrittenOrReadOn: Boolean = false
}

object Value {
    object B
}