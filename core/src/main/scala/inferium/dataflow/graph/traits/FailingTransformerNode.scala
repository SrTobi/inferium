package inferium.dataflow.graph.traits

import inferium.dataflow.graph.{LinearNode, Node}
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

abstract class FailingTransformerNode(implicit info: Node.Info) extends LinearNode with SingleSuccessor {

    override def successors: Seq[Node] = super.successors ++ info.catchTarget

    override final def process(implicit analysis: DataFlowAnalysis): Unit = {
        val outState = transform(inState)
        outState foreach { succ <~ _ }
    }

    protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState]
}
