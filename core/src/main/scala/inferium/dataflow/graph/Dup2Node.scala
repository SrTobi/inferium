package inferium.dataflow.graph

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.traits.TransformerNode

class Dup2Node(implicit _info: Node.Info) extends TransformerNode {

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        var fst :: snd :: rest = state.stack

        state.copy(stack = fst :: snd :: fst :: snd :: rest)
    }

    override def asAsmStmt: String = s"dup2"
}
