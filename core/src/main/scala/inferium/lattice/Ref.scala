package inferium.lattice

import inferium.utils.macros.blockRec

case class Ref(base: Entity, property: String, target: Option[ValueLocation], mightBeAbsent: AbsentLattice) extends Entity {
    assert(target.isDefined || mightBeAbsent == AbsentLattice.MightBeAbsent)

    override def isNormalized: Boolean = false

    @blockRec(NeverValue)
    override def normalized(heap: Heap.Mutator): Entity = {
        target map {
            t =>
                val inner = heap.getValue(t).normalized(heap)
                if (mightBeAbsent.asBool) UnionValue(inner, UndefinedValue) else inner
        } getOrElse UndefinedValue
    }

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectEntity] = normalized(heap).coerceToObjects(heap)
}