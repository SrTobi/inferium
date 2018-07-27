package inferium.lattice

import inferium.dataflow.CallableInfo
import inferium.lattice.assertions.Assertion.NoAssertionEffect
import inferium.lattice.assertions.{Assertion, Propertyfied}
import inferium.utils.macros.blockRec

case class Ref(base: Entity, property: String, target: Set[ValueLocation]) extends Entity {
    assert(target.nonEmpty)

    override def isNormalized: Boolean = false

    @blockRec(nonrec = true)
    private def resolve(heap: Heap.Mutator): Seq[Entity] = target.toSeq map { heap.getValue(_).normalized(heap) }

    @blockRec(NeverValue)
    override def normalized(heap: Heap.Mutator): Entity = {
        UnionValue(resolve(heap))
    }


    @blockRec(GeneralBoolLattice.Bottom)
    override def asBoolLattice(heap: Heap.Mutator): GeneralBoolLattice = {
        GeneralBoolLattice.unify(target.iterator map { t => heap.getValue(t).asBoolLattice(heap) })
    }

    @blockRec(StringLattice.Bottom)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = {
        StringLattice.unify(target.iterator map { t => heap.getValue(t).asStringLattice(heap) })
    }

    @blockRec(Set.empty)
    override def asTypeof(heap: Heap.Mutator): Set[String] = {
        target.iterator.flatMap(heap.getValue(_).asTypeof(heap)).toSet
    }

    @blockRec(Seq.empty)
    override def asProbes(heap: Heap.Mutator): Seq[ProbeEntity] = {
        target.iterator.flatMap(heap.getValue(_).asProbes(heap)).toSeq
    }

    //@blockRec(nonrec = true)
    //override def withAssertion(cond: Entity => Boolean, heap: Heap.Mutator): Ref = ???

    @blockRec((NeverValue, false, Assertion.noEffect(NeverValue)))
    protected[lattice] override def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Boolean, Assertion.Effect) = {

        //def filterBase(obj: Entity): Boolean = true
        //base.instituteAssertion(filterBase, heap)
        assertion match {
            case assertions.Propertyfied(_, _) =>
            case assertions.Truthyfied =>
                base.instituteAssertion(Propertyfied(property, has = true), heap)
            case assertions.Falsyfied =>
                base.instituteAssertion(Propertyfied(property, has = false), heap)
        }


        var hasChanged = false
        var changedEntityEffects: Assertion.Effect = null
        var remainingEntities = 0

        val results = target.toSeq.map { loc =>
            val oldValue = heap.getValue(loc)
            val (assertedValue, changed, effects) = oldValue.gatherAssertionEffects(assertion, heap)

            if (changed) {
                hasChanged = true
                if (changedEntityEffects == null && assertedValue != NeverValue) {
                    changedEntityEffects = () => {
                        val affectedValue = effects()
                        heap.changeValue(loc, affectedValue)
                        this
                    }
                }
            }
            if (assertedValue != NeverValue) {
                remainingEntities += 1
            }

            assertedValue
        }

        lazy val result = Entity.unify(results)
        if (!hasChanged) {
            (this, false, Assertion.noEffect(this))
        } else if (remainingEntities == 1 && changedEntityEffects != null) {
            (result, true, changedEntityEffects)
        } else {
            (result, true, Assertion.noEffect(result))
        }
    }

    override def coerceToCallables(heap: Heap.Mutator, fail: () => Unit): Seq[Callable] = normalized(heap).coerceToCallables(heap, fail)

    override def coerceToConstructionObject(heap: Heap.Mutator, constructionObject: ObjectLike): Seq[ObjectLike] = normalized(heap).coerceToConstructionObject(heap, constructionObject)

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = normalized(heap).coerceToObjects(heap)

    override def toString: String = s"Ref[$base.$property -> {${target.mkString(", ")}}]"
}