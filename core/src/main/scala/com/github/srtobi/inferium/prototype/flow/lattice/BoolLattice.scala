package com.github.srtobi.inferium.prototype.flow.lattice

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice.Top


sealed abstract class GeneralBoolLattice {

    def unify(other: GeneralBoolLattice): GeneralBoolLattice
}

sealed abstract class BoolLattice extends GeneralBoolLattice {
    import BoolLattice._

    def unify(other: GeneralBoolLattice): BoolLattice = if (this == other || other == GeneralBoolLattice.Bottom) this else Top
    def unify(other: BoolLattice): BoolLattice = if (this == other) this else Top
}

object BoolLattice {
    case object Top extends BoolLattice
    case object True extends BoolLattice
    case object False extends BoolLattice

    def apply(value: Boolean): BoolLattice = if (value) True else False
}

object GeneralBoolLattice {
    case object Bottom extends GeneralBoolLattice {
        override def unify(other: GeneralBoolLattice): GeneralBoolLattice = other
    }
}