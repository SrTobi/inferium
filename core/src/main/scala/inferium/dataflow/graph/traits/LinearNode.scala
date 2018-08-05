package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node
import inferium.dataflow.graph.Node.NodeId
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.Location

abstract class LinearNode(implicit _info: Node.Info) extends Node with SinglePredecessor {
    private var _inState: ExecutionState = _

    def inState: ExecutionState = _inState

    override def reset(): Unit = _inState = null

    override def setNewInState(state: ExecutionState, origin: NodeId)(implicit analysis: DataFlowAnalysis): Unit = {
        checkLexicalFrame(state.lexicalFrame)
        if (inState != state) {
            _inState = state
            analysis.enqueue(this)
        }
    }
}
