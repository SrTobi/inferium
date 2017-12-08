package com.github.srtobi.inferium.prototype.flow

trait Solver {
    //def newValue(): Value

    def newEmptyObject(): EmptyObject

    def undefined(): UndefinedValue
    def nullValue(): NullValue

    def boolean(): BoolValue
    def boolean(specific: Boolean): BoolValue
    def number(): NumberValue
    def number(specific: String): NumberValue
    def string(): StringValue
    def string(specific: String): StringValue
}
