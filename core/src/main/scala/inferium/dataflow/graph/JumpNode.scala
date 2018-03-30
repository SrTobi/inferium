package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

class JumpNode extends Node with SingleSuccessor with SinglePredecessor {
    private var _target: Node = _

    def target: Node = {
        assert(_target != null)
        _target
    }

    def target_=(node: Node): Unit = {
        assert(_target == null)
        assert(node != null)
        _target = node
    }

    override def setNewInState(state: ExecutionState): Unit = {
        target.setNewInState(state)
    }

    override def process(): Unit = {
        throw new IllegalStateException("jump nodes should not be processed")
    }
}
