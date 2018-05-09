package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

class DupNode(val times: Int)(implicit _info: Node.Info) extends TransformerNode {
    assert(times > 0)

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        var stack = state.stack
        val top = stack.head

        for (_ <- 1 to times) {
            stack = top :: stack
        }
        state.copy(stack)
    }

    override def asAsmStmt: String = s"dup $times"
}
