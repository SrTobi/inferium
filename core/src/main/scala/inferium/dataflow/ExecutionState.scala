package inferium.dataflow

import inferium.lattice.Entity
import inferium.memory.Heap


case class ExecutionState(heap: Heap, stack: ExecutionState.Stack)(val dataFlowAnalysis: DataFlowAnalysis) {


    def flowTo(node: graph.Node): Unit = {
        dataFlowAnalysis.contextFlowsTo(this, node)
    }
}

object ExecutionState {
    type Stack = List[Entity]
}