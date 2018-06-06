package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice._

trait HeapReading extends Node {
    private val heapResolveLoc = Location()
    private val heapReadingLoc = Location()

    protected final def read(target: Entity, propertyName: String, state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[(Entity, ExecutionState)] = {
        val initialHeap = state.heap

        val resolveMutator = initialHeap.begin(heapResolveLoc)
        val objs = target.coerceToObjects(resolveMutator)
        val heapAfterCoersion = initialHeap.end(resolveMutator)

        if (objs == Seq()) {
            // todo: generate exception object
            fail(state.copy(heap = heapAfterCoersion), UndefinedValue)
            return None
        }

        val readMutator = heapAfterCoersion.begin(heapReadingLoc)
        val result = UnionValue(objs map { read(target, _, propertyName, readMutator) })
        val resultHeap = heapAfterCoersion.end(readMutator)

        val resultState = state.copy(heap = resultHeap)
        Some((result, resultState))
    }

    /*protected final def read(obj: ObjectLike, propertyName: String, state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[(Entity, ExecutionState)] = {
        val heap = state.heap

        val readMutator = heap.begin(heapReadingLoc)
        val result = read(obj, obj, propertyName, readMutator)
        val resultHeap = heap.end(readMutator)

        val resultState = state.copy(heap = resultHeap)
        Some((result, resultState))
    }*/

    private final def read(base: Entity, obj: ObjectLike, propertyName: String, mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Entity = {

        mutator.getProperty(obj, propertyName) match {
            case ConcreteProperty(_, _, target, _, getter, _) =>
                if (getter.nonEmpty) {
                    // todo: implement getters
                    ???
                }

                Ref(base, propertyName, target)

            case AbstractProperty(_, _, value, _, getter, _, mightBeAbsent) =>
                if (getter != NeverValue) {
                    // todo: implement getters
                    ???
                }
                if (mightBeAbsent) UndefinedValue | value else value
        }
    }
}
