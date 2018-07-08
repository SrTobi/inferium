package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.{Entity, Heap}

trait Async[-Result] {
    protected final def complete(result: Result, state: ExecutionState, analysis: DataFlowAnalysis): Unit = {
        onComplete(result, state, analysis)
    }
    protected def onComplete(result: Result, state: ExecutionState, analysis: DataFlowAnalysis): Unit
}

object Async {
    trait CompleteWithSuccessor extends Async[Any] with Origin {
        def succ: Node
        override def onComplete(result: Any, state: ExecutionState, analysis: DataFlowAnalysis): Unit = {
            succ.setNewInState(state, thisOrigin)(analysis)
        }
    }
}