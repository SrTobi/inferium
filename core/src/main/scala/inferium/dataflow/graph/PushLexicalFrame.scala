package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.ObjectEntity

class PushLexicalFrame(implicit _info: Node.Info) extends TransformerNode {

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val heap = state.heap
        val mutator = heap.begin(loc)
        val lexObj = mutator.allocObject(loc)
        val newHeap = heap.end(mutator)
        val newLexFrame = lexObj :: state.lexicalFrame
        state.copy(heap = newHeap, lexicalFrame = newLexFrame)
    }

    override def asAsmStmt: String = "lexPush"
}
