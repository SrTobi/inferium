package inferium.dataflow.graph

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice._

trait HeapWriting extends FailingTransformerNode {
    private val heapWritingLoc = Location()
    private val heapResolveLoc = Location()
    private val valueLocation = ValueLocation(Location())

    protected final def write(target: Entity, propertyName: String, value: Entity, state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[(Entity, ExecutionState)] = {
        val initialHeap = state.heap

        val resolveMutator = initialHeap.begin(heapResolveLoc)
        val objs = target.coerceToObjects(resolveMutator)
        val heapAfterCoersion = initialHeap.end(resolveMutator)

        if (objs == Seq()) {
            // todo: generate exception object
            fail(state.copy(heap = heapAfterCoersion), UndefinedValue)
            return None
        }

        val writeMutator = heapAfterCoersion.begin(heapWritingLoc)
        val isCertainWrite = objs.tail.isEmpty
        var propertyChanged = false
        objs foreach { obj =>
            val res = write(target, obj, propertyName, value, isCertainWrite, writeMutator)
            if (res) {
                propertyChanged = true
            }
        }
        if (propertyChanged) {
            writeMutator.setValue(valueLocation, value)
        }
        val resultHeap = heapAfterCoersion.end(writeMutator)

        val resultState = state.copy(heap = resultHeap)
        val result = Ref(target, propertyName, if(propertyChanged) Some(valueLocation) else None, AbsentLattice.NeverAbsent)
        Some((result, resultState))
    }

    /*protected def write(target: ObjectEntity, propertyName: String, value: Entity): Entity = {

    }*/

    // returns whether a property was changed
    private def write(base: Entity, obj: ObjectEntity, propertyName: String, value: Entity, onlyOneTarget: Boolean, mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Boolean = {
        val isCertainWrite = onlyOneTarget && true // todo: the object might be abstrict so it wouldn't be a certain write

        val result =
            mutator.getProperty(obj, propertyName) match {
                case AbsentProperty =>
                    val absent = if (isCertainWrite) AbsentLattice.NeverAbsent else AbsentLattice.MightBeAbsent
                    Property.defaultWriteToObject(Set(valueLocation), absent)
                    //Ref(base, propertyName, None, AbsentLattice.MightBeAbsent)

                case p@Property(_, _, oldAbsent, oldValues, _, _, setter) =>

                    if (setter.nonEmpty) {
                        // todo: implement setter
                        ???
                    }

                    if (isCertainWrite) {
                        Property.defaultWriteToObject(Set(valueLocation), AbsentLattice.NeverAbsent)
                    } else {
                        p.copy(value = oldValues + valueLocation)
                    }
            }

        mutator.setProperty(obj, propertyName, result)

        true
    }
}
