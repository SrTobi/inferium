package inferium.lattice

import inferium.lattice.BoolLattice.Top


sealed abstract class GeneralBoolLattice {
    def unify(other: GeneralBoolLattice): GeneralBoolLattice

    def mightBe(bool: Boolean): Boolean = if (bool) mightBeTrue else mightBeFalse
    def mightBeTrue: Boolean
    def mightBeFalse: Boolean
}

sealed abstract class BoolLattice extends GeneralBoolLattice {
    import BoolLattice._

    def unify(other: GeneralBoolLattice): BoolLattice = if (this == other || other == GeneralBoolLattice.Bottom) this else Top
    def unify(other: BoolLattice): BoolLattice = if (this == other) this else Top
}

object BoolLattice {
    case object Top extends BoolLattice {
        override def mightBeTrue: Boolean = true
        override def mightBeFalse: Boolean = true
    }

    case object True extends BoolLattice {
        override def mightBeTrue: Boolean = true
        override def mightBeFalse: Boolean = false
    }

    case object False extends BoolLattice {
        override def mightBeTrue: Boolean = false
        override def mightBeFalse: Boolean = true
    }

    def apply(value: Boolean): BoolLattice = if (value) True else False

    def unify(bools: Iterable[BoolLattice]): BoolLattice = {

        bools.reduce {
            (a, b) =>
                if (a == Top)
                    return Top
                else
                    a.unify(b)
        }
    }
}

object GeneralBoolLattice {
    case object Bottom extends GeneralBoolLattice {
        override def unify(other: GeneralBoolLattice): GeneralBoolLattice = other

        override def mightBeTrue: Boolean = false
        override def mightBeFalse: Boolean = false
    }

    def unify(bools: TraversableOnce[GeneralBoolLattice]): GeneralBoolLattice = {

        bools.foldLeft[GeneralBoolLattice](Bottom) {
            (a, b) =>
                if (a == Top)
                    return Top
                else
                    a.unify(b)
        }
    }
}