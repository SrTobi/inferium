package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

abstract class TransformerNode(implicit info: Node.Info) extends LinearNode with SingleSuccessor {
    override final def process(implicit analysis: DataFlowAnalysis): Unit = {
        val outState = transform(inState)
        succ <~ outState
    }

    protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState
}
