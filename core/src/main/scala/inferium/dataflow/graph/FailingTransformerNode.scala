package inferium.dataflow.graph

import inferium.dataflow.ExecutionState

abstract class FailingTransformerNode extends LinearNode {
    override final def process(): Unit = {
        val outState = transform(inState)
        outState foreach { succ <~ _ }
    }

    protected def transform(state: ExecutionState): Option[ExecutionState]
}
