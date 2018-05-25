package inferium.lattice

import scala.language.implicitConversions

final class AbsentLattice(val mightBeAbsent: Boolean) extends AnyVal {
    def asBool: Boolean = mightBeAbsent

    def unify(other: AbsentLattice): AbsentLattice = AbsentLattice(mightBeAbsent || other.mightBeAbsent)

    override def toString: String = if (mightBeAbsent) "MightBeAbsent" else "NeverAbsent"
}

object AbsentLattice {
    def apply(mightBeAbsent: Boolean): AbsentLattice = new AbsentLattice(mightBeAbsent)

    val MightBeAbsent: AbsentLattice = AbsentLattice(true)
    val NeverAbsent: AbsentLattice = AbsentLattice(false)

    implicit def toBoolean(lattice: AbsentLattice): Boolean = lattice.asBool
}
