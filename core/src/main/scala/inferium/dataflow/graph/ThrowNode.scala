package inferium.dataflow.graph

import inferium.dataflow.graph.Node.NodeId
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.traits.{SinglePredecessor, SingleSuccessor}

class ThrowNode()(implicit _info: Node.Info) extends Node with SingleSuccessor with SinglePredecessor{
    override def setNewInState(state: ExecutionState, origin: NodeId)(implicit analysis: DataFlowAnalysis): Unit = {
        val exception :: restStack = state.stack
        fail(state.copy(stack = restStack), exception)
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = throw new IllegalStateException("throw nodes should not be processed")

    override def asAsmStmt: String = "throw"
}
