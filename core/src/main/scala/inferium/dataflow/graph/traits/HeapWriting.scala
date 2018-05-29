package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice._

trait HeapWriting extends Node {
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
        val result = if (propertyChanged) Ref(target, propertyName, Set(valueLocation)) else value

        Some((result, resultState))
    }

    /*protected def write(target: ObjectLike, propertyName: String, value: Entity): Entity = {

    }*/

    // returns whether a property was changed
    private def write(base: Entity, obj: ObjectLike, propertyName: String, value: Entity, onlyOneTarget: Boolean, mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Boolean = {
        val isCertainWrite = onlyOneTarget && mutator.isConcreteObject(obj) // todo: the object might be abstrict so it wouldn't be a certain write

        val p@Property(_, _, oldValues, _, _, setter) = mutator.getProperty(obj, propertyName)
        if (setter.nonEmpty) {
            // todo: implement setter
            ???
        }

        val result = if (isCertainWrite) {
            Property.defaultWriteToObject(Set(valueLocation))
        } else {
            p.copy(value = oldValues + valueLocation)
        }

        mutator.setProperty(obj, propertyName, result)

        true
    }
}
