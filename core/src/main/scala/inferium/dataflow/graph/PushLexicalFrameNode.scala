package inferium.dataflow.graph
import inferium.dataflow.graph.traits.TransformerNode
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.ObjectLike

class PushLexicalFrameNode(val kind: String, val takeFromStack: Boolean)(implicit _info: Node.Info) extends TransformerNode {

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val heap = state.heap
        val stack = state.stack

        val mutator = heap.begin(loc)
        val (lexObj, restStack) = if (takeFromStack) {
            val lexObj :: restStack = stack
            (lexObj, restStack)
        } else {
            val lexObj = mutator.allocOrdinaryObject(loc)
            (lexObj, stack)
        }
        val newHeap = heap.end(mutator)

        val newLexFrame = lexObj :: state.lexicalFrame
        state.copy(heap = newHeap, lexicalFrame = newLexFrame)
    }

    override def asAsmStmt: String = s"lexPush <$kind>"
}
