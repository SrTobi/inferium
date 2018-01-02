package com.github.srtobi.inferium.prototype.flow

trait Solver {
    //def newValue(): Value

    def newEmptyObject(): ObjectValue

    def boolean(): BoolValue
    def boolean(specific: Boolean): BoolValue
    def number(): NumberValue
    def number(specific: String): NumberValue
    def string(): StringValue
    def string(specific: String): StringValue
}

object Solver extends Solver {
    override def newEmptyObject(): ObjectValue = new ObjectValue
    override def boolean(): BoolValue = ???
    override def boolean(specific: Boolean): BoolValue = BoolValue(specific)
    override def number(): NumberValue = ???
    override def number(specific: String): NumberValue = NumberValue(specific.toInt)
    override def string(): StringValue = ???
    override def string(specific: String): StringValue = StringValue(specific)
}