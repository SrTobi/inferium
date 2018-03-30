package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

abstract class TransformerNode extends LinearNode {
    override final def process(): Unit = {
        val outState = transform(inState)
        succ <~ outState
    }

    protected def transform(state: ExecutionState): ExecutionState
}
