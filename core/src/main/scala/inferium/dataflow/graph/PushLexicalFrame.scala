package inferium.dataflow.graph
import inferium.dataflow.ExecutionState
import inferium.lattice.ObjectEntity

class PushLexicalFrame(implicit _info: Node.Info) extends TransformerNode {

    private val lexObj = ObjectEntity.ordinary(loc)

    override protected def transform(state: ExecutionState): ExecutionState = {
        val heap = state.heap
        val mutator = heap.begin(loc)
        mutator.allocObject(lexObj)
        heap.end(mutator)
        state.copy(lexicalFrame = lexObj :: state.lexicalFrame)
    }

    override def asAsmStmt: String = "lexPush"
}
