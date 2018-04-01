package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

abstract class TransformerNode(implicit info: Node.Info) extends LinearNode with SingleSuccessor {
    override final def process(): Unit = {
        val outState = transform(inState)
        succ <~ outState
    }

    protected def transform(state: ExecutionState): ExecutionState
}
