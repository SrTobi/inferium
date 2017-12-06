package com.github.srtobi.inferium.prototype.flow

trait Solver {
    //def newValue(): Value

    def newEmptyObject(): Value

    def undefined(): Value
    def boolean(): Value
    def boolean(specific: Boolean): Value
    def number(): Value
    def number(specific: String): Value
    def string(): Value
    def string(specific: String): Value

    def function(): Value
}
