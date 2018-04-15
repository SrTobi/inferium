package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.ObjectEntity

class PushLexicalFrame(implicit _info: Node.Info) extends TransformerNode {

    private val lexObj = ObjectEntity.ordinary(loc)

    override protected def transform(state: ExecutionState, analysis: DataFlowAnalysis): ExecutionState = {
        val heap = state.heap
        val mutator = heap.begin(loc)
        mutator.allocObject(lexObj)
        val newHeap = heap.end(mutator)
        val newLexFrame = lexObj :: state.lexicalFrame
        state.copy(heap = newHeap, lexicalFrame = newLexFrame)
    }

    override def asAsmStmt: String = "lexPush"
}
