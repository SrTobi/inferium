package inferium.lattice

import inferium.lattice.assertions.{Assertion, Propertyfied}
import inferium.utils.macros.blockRec

case class Ref(base: Entity, property: String, target: Set[ValueLocation]) extends Entity {
    assert(target.nonEmpty)

    override def isNormalized: Boolean = false

    @blockRec(NeverValue)
    override def normalized(heap: Heap.Mutator): Entity = {
        UnionValue(target.toSeq map { heap.getValue(_).normalized(heap) })
    }


    @blockRec(GeneralBoolLattice.Bottom)
    override def asBoolLattice(heap: Heap.Mutator): GeneralBoolLattice = {
        GeneralBoolLattice.unify(target map { t => heap.getValue(t).asBoolLattice(heap) })
    }

    //@blockRec(nonrec = true)
    //override def withAssertion(cond: Entity => Boolean, heap: Heap.Mutator): Ref = ???

    @blockRec(nonrec = true)
    protected[lattice] override def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Iterator[() => Unit]) = {

        //def filterBase(obj: Entity): Boolean = true
        //base.instituteAssertion(filterBase, heap)
        assertion match {
            case assertions.Propertyfied(_, _) =>
            case assertions.Truthyfied =>
                base.instituteAssertion(Propertyfied(property, has = true), heap)
            case assertions.Falsyfied =>
                base.instituteAssertion(Propertyfied(property, has = false), heap)
        }

        val (results, hereEffects) = target.toSeq.map { loc =>
            val oldValue = heap.getValue(loc)
            val (assertedValue, effects) = oldValue.gatherAssertionEffects(assertion, heap)
            val changed = oldValue ne assertedValue

            (assertedValue, () => {
                effects.foreach(_())

                if (changed) {
                    heap.setValue(loc, assertedValue)
                }
            })
        }.unzip

        val result = UnionValue(results)

        if (result == NeverValue) {
            (NeverValue, Iterator())
        } else {
            (this, hereEffects.iterator)
        }
    }

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectEntity] = normalized(heap).coerceToObjects(heap)

    override def toString: String = s"Ref[$base.$property -> {${target.mkString(", ")}}]"
}