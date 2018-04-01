package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

class PopNode(implicit _info: Node.Info) extends TransformerNode {
    override def transform(state: ExecutionState): ExecutionState = {
        state.copy(stack = state.stack.tail)
    }

    override def toString: String = "pop"
}
