package inferium.lattice

case class Ref(base: Slot, property: String, target: Set[Slot]) extends Entity