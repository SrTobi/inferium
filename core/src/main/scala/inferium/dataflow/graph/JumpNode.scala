package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

class JumpNode(val target: Node)(implicit _info: Node.Info) extends Node with SingleSuccessor with SinglePredecessor {
    assert(target != null)
    target.addPredecessor(this)

    override def successors: Traversable[Node] = super.successors ++ Seq(target)

    override def setNewInState(state: ExecutionState): Unit = {
        target <~ state
    }

    override def process(): Unit = {
        throw new IllegalStateException("jump nodes should not be processed")
    }
}
