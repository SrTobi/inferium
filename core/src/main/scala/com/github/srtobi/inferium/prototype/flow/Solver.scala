package com.github.srtobi.inferium.prototype.flow

trait Solver {
    //def newValue(): Value

    def newEmptyObject(): ObjectValue

    def boolean(): SpecificBoolValue
    def boolean(specific: Boolean): SpecificBoolValue
    def number(): SpecificNumberValue
    def number(specific: String): SpecificNumberValue
    def string(): SpecificStringValue
    def string(specific: String): SpecificStringValue
}

object Solver extends Solver {
    override def newEmptyObject(): ObjectValue = new ObjectValue
    override def boolean(): SpecificBoolValue = ???
    override def boolean(specific: Boolean): SpecificBoolValue = SpecificBoolValue(specific)
    override def number(): SpecificNumberValue = ???
    override def number(specific: String): SpecificNumberValue = SpecificNumberValue(specific.toInt)
    override def string(): SpecificStringValue = ???
    override def string(specific: String): SpecificStringValue = SpecificStringValue(specific)
}