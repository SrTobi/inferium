package inferium.dataflow.graph

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

abstract class FailingTransformerNode(implicit info: Node.Info) extends LinearNode with SingleSuccessor {

    override def successors: Traversable[Node] = info.catchTarget ++ super.successors

    override final def process(implicit analysis: DataFlowAnalysis): Unit = {
        val outState = transform(inState)
        outState foreach { succ <~ _ }
    }

    protected def transform(state: ExecutionState): Option[ExecutionState]
}
