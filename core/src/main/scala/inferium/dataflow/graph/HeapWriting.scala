package inferium.dataflow.graph

import inferium.dataflow.ExecutionState
import inferium.lattice._

trait HeapWriting extends FailingTransformerNode {
    private val heapWritingLoc = Location()
    private val valueLocation = ValueLocation(Location())

    protected final def write(target: Entity, propertyName: String, value: Entity, state: ExecutionState): Option[ExecutionState] = {
        ???
    }

    protected final def write(objs: Seq[ObjectEntity], objsOrigin: ValueLocation, propertyName: String, value: Entity, state: ExecutionState): Option[ExecutionState] = {
        assert(objs.nonEmpty)
        val heap = state.heap
        val heapMutator = heap.begin(heapWritingLoc)
        val isCertainWrite = objs.length == 1
        val absent = if (isCertainWrite) AbsentLattice.NeverAbsent else AbsentLattice.MightBeAbsent

        objs foreach {
            obj =>
                val property = heapMutator.getProperty(obj, propertyName) match {
                    case AbsentProperty =>
                        // property was not defined
                        Property.defaultWriteToObject(Set(valueLocation), absent)

                    case p@Property(_, _, mightBeAbsent, oldValues, _, _, setter) =>

                        if (setter.nonEmpty) {
                            // todo: implement setter
                            ???
                        }
                        p.copy(value = oldValues + valueLocation, mightBeAbsent = mightBeAbsent unify absent)
                }

                // set the new property and also set fill the value location
                heapMutator.setProperty(obj, propertyName, property)
                heapMutator.setValue(valueLocation, value)
        }

        val resultState = state.copy(heap = heap.end(heapMutator))
        Some(resultState)
    }
}
