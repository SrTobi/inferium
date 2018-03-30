package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

class PopNode extends TransformerNode {
    override def transform(state: ExecutionState): ExecutionState = {
        state.copy(stack = state.stack.tail)
    }
}
