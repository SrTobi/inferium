package inferium.lattice

case class Ref(base: ValueLocation, property: String, target: Set[ValueLocation], mightBeAbsent: AbsentLattice) extends Entity