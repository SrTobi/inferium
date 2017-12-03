package com.github.srtobi.inferium.prototype.flow.lattice

trait Lattice[T] {
    def unify(other: T): T
}

object Lattice {
    def unify[T <: Lattice[T]](left: T, right: T): T = left.unify(right)
}
