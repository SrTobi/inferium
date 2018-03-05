package inferium.lattice


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

    def unify(bools: Iterable[BoolLattice]): BoolLattice = {
        val it = bools.iterator
        var result: BoolLattice = it.next()
        for (bool <- it) {
            result = result.unify(bool)
            if (result == BoolLattice.Top) {
                return BoolLattice.Top
            }
        }
        return result
    }
}

object GeneralBoolLattice {
    case object Bottom extends GeneralBoolLattice {
        override def unify(other: GeneralBoolLattice): GeneralBoolLattice = other
    }
}