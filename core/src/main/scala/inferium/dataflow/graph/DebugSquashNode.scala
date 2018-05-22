package inferium.dataflow.graph

import inferium.dataflow.graph.traits.TransformerNode
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.UnionValue

class DebugSquashNode(val squashNum: Int)(implicit _info: Node.Info) extends TransformerNode {
    assert(squashNum >= 2)

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val stack = state.stack

        val (args, restStack) = stack.splitAt(squashNum)
        val result = UnionValue(args)

        state.copy(stack = result :: restStack)
    }

    override def asAsmStmt: String = s"dbg.squash $squashNum"
}
