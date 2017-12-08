package com.github.srtobi.inferium.prototype.flow.lattice



sealed abstract class BoolLattice extends Lattice[BoolLattice] {
    import BoolLattice._

    def unify(other: BoolLattice): BoolLattice = if (this == other) this else (this, other) match {
        case (Bottom, o) => o
        case (o, Bottom) => o
        case _ => Top
    }
}

object BoolLattice {
    case object Top extends BoolLattice
    case object True extends BoolLattice
    case object False extends BoolLattice
    case object Bottom extends BoolLattice

    def apply(value: Boolean): BoolLattice = if (value) True else False
}