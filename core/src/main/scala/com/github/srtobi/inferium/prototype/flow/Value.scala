package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

trait Value {
    def flowsTo(upperBound: Value): Unit

    def boolLike: BoolLattice
}

object Value {
    object B
}