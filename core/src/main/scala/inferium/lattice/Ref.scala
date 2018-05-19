package inferium.lattice

import inferium.lattice.assertions.{Assertion, HasProperty}
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

    //@blockRec(nonrec = true)
    //override def withAssertion(cond: Entity => Boolean, heap: Heap.Mutator): Ref = ???

    @blockRec(nonrec = true)
    override def instituteAssertion(assertion: Assertion, heap: Heap.Mutator, alone: Boolean): Ref = {

        // todo: assert that the base has the property
        //def filterBase(obj: Entity): Boolean = true
        //base.instituteAssertion(filterBase, heap)
        base.instituteAssertion(HasProperty(property), heap, alone = true)

        for (loc <- target) {
            val value = heap.getValue(loc)
            val assertedValue = value.instituteAssertion(assertion, heap, alone)
            if (value ne assertedValue) {
                heap.setValue(loc, assertedValue)
            }
        }


        if (mightBeAbsent) copy(mightBeAbsent = AbsentLattice.NeverAbsent) else this
    }

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectEntity] = normalized(heap).coerceToObjects(heap)
}