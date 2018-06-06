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
        var valueLocationWasWritten = false
        objs foreach { obj =>
            val res = write(target, obj, propertyName, value, isCertainWrite, writeMutator)
            if (res) {
                valueLocationWasWritten = true
            }
        }
        if (valueLocationWasWritten) {
            writeMutator.setValue(valueLocation, value)
        }
        val resultHeap = heapAfterCoersion.end(writeMutator)

        val resultState = state.copy(heap = resultHeap)
        val result = if (valueLocationWasWritten) Ref(target, propertyName, Set(valueLocation)) else value

        Some((result, resultState))
    }

    /*protected def write(target: ObjectLike, propertyName: String, value: Entity): Entity = {

    }*/

    // returns whether a property was changed
    private def write(base: Entity, obj: ObjectLike, propertyName: String, value: Entity, onlyOneTarget: Boolean, mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Boolean = {

        mutator.writeToProperty(obj, propertyName, valueLocation, isCertainWrite = onlyOneTarget, value) match {
            case AbstractProperty(_, _, _, _, _, setter, _) =>
                if (setter != NeverValue) {
                    ???
                }
                false

            case ConcreteProperty(_, _, _, _, _, setter) =>
                if (setter.nonEmpty) {
                    // todo: implement setter
                    ???
                }
                true
        }
    }
}
