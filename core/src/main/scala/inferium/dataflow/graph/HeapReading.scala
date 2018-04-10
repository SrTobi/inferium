package inferium.dataflow.graph

import inferium.dataflow.ExecutionState
import inferium.lattice._

trait HeapReading extends FailingTransformerNode {
    private val heapReadingLoc = Location()

    protected final def read(entity: Entity, propertyName: String, state: ExecutionState): Option[(Entity, ExecutionState)] = {
        ???
    }

    protected final def read(objs: TraversableOnce[ObjectEntity], objsOrigin: ValueLocation, propertyName: String, state: ExecutionState): Option[(Ref, ExecutionState)] = {
        val heap = state.heap
        val heapMutator = heap.begin(heapReadingLoc)
        val (resultValues, resultAbsent) = objs.foldLeft((Set.empty[ValueLocation], AbsentLattice.NeverAbsent)) {
            case ((values, accAbsent), obj) =>
                heapMutator.getProperty(obj, propertyName) match {
                    case AbsentProperty =>
                        (values, AbsentLattice.MightBeAbsent)

                    case Property(_, _, mightBeAbsent, value, _, getter, _) =>

                        if (getter.nonEmpty) {
                            // todo: implement getters
                            ???
                        }
                        (values | value, mightBeAbsent unify accAbsent)
                }
        }
        val result = Ref(objsOrigin, propertyName, resultValues, resultAbsent)
        val resultState = state.copy(heap = heap.end(heapMutator))
        Some((result, resultState))
    }
}
