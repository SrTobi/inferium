package inferium.lattice

case class Ref(base: ValueLocation, property: String, target: Set[ValueLocation], mightBeAbsent: AbsentLattice) extends Entity {
    override def isNormalized: Boolean = false
    override def normalized(heap: Heap.Mutator): Entity = {
        Entity.unify(target map { heap.getValue } map { _.normalized(heap) } toSeq)
    }
}