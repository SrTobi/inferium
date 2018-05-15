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


    @blockRec(GeneralBoolLattice.Bottom)
    override def asBoolLattice(heap: Heap.Mutator): GeneralBoolLattice = {
        lazy val absent = if(mightBeAbsent) BoolLattice.False else GeneralBoolLattice.Bottom
        target map { t => heap.getValue(t).asBoolLattice(heap) } map { _ unify absent } getOrElse BoolLattice.False
    }

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectEntity] = normalized(heap).coerceToObjects(heap)
}