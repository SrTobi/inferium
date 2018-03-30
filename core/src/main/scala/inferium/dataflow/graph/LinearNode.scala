package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

abstract class LinearNode extends Node with SingleSuccessor with SinglePredecessor {
    private var _inState: ExecutionState = _

    def inState: ExecutionState = _inState

    override def setNewInState(state: ExecutionState): Unit = {
        if (inState != state) {
            _inState = state
            state.flowTo(this)
        }
    }
}
