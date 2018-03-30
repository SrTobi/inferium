package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

class LexicalContextReadNode(val varName: String) extends FailingTransformerNode {
    override protected def transform(state: ExecutionState): Option[ExecutionState] = {
        // do the read and push it onto the stack
        ???
    }
}
