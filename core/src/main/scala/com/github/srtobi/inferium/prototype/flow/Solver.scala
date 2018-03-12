package com.github.srtobi.inferium.prototype.flow

// not really needed :D
trait Solver {
    //def newValue(): Value

    def newEmptyObject(): ObjectValue

    def boolean(): BoolValue
    def boolean(specific: Boolean): SpecificBoolValue
    def number(): NumberValue
    def number(specific: String): SpecificNumberValue
    def string(): StringValue
    def string(specific: String): SpecificStringValue
}

object Solver extends Solver {
    override def newEmptyObject(): ObjectValue = new ObjectValue
    override def boolean(): BoolValue = BoolValue
    override def boolean(specific: Boolean): SpecificBoolValue = SpecificBoolValue(specific)
    override def number(): NumberValue = NumberValue
    override def number(specific: String): SpecificNumberValue = SpecificNumberValue(specific.toInt)
    override def string(): StringValue = StringValue
    override def string(specific: String): SpecificStringValue = SpecificStringValue(specific)
}