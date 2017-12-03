package com.github.srtobi.inferium.prototype.flow.lattice



sealed abstract class BoolLattice extends Lattice[BoolLattice] {
    import BoolLattice._

    def unify(other: BoolLattice): BoolLattice = if (this == other) this else Top
}

object BoolLattice {
    case object Top extends BoolLattice
    case object True extends BoolLattice
    case object False extends BoolLattice
    case object Bottom extends BoolLattice
}