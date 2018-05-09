package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

class PopNode(implicit _info: Node.Info) extends TransformerNode {
    override def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        state.copy(stack = state.stack.tail)
    }

    override def asAsmStmt: String = "pop"
}
