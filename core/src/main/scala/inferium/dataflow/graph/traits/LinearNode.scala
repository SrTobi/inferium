package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

abstract class LinearNode(implicit _info: Node.Info) extends Node with SinglePredecessor {
    private var _inState: ExecutionState = _

    def inState: ExecutionState = _inState

    override def setNewInState(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Unit = {
        if (inState != state) {
            _inState = state
            analysis.enqueue(this)
        }
    }
}
