package inferium.dataflow.graph

import inferium.dataflow.graph.traits.TransformerNode
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.Entity

class LiteralNode(val literal: Entity)(implicit _info: Node.Info) extends TransformerNode {
    override def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val newStack = literal :: state.stack
        return state.copy(stack = newStack)
    }

    override def toString: String = s"[$id]literal[$literal]"

    override def asAsmStmt: String = s"push $literal"
}
