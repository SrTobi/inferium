package inferium.dataflow.graph

import inferium.dataflow.ExecutionState
import inferium.lattice.Entity

class LiteralNode(literal: Entity) extends TransformerNode {
    override def transform(state: ExecutionState): ExecutionState = {
        val newStack = literal :: state.stack
        return state.copy(stack = newStack)
    }
}
