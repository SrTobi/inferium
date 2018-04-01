package inferium.dataflow.graph

import inferium.dataflow.ExecutionState
import inferium.lattice.Entity

class LiteralNode(val literal: Entity)(implicit _info: Node.Info) extends TransformerNode {
    override def transform(state: ExecutionState): ExecutionState = {
        val newStack = literal :: state.stack
        return state.copy(stack = newStack)
    }

    override def toString: String = s"literal[$literal]"
}
