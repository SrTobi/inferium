package inferium.dataflow.graph
import inferium.dataflow.ExecutionState
import inferium.lattice.ObjLocation

class PushLexicalFrame(implicit _info: Node.Info) extends TransformerNode {
    override protected def transform(state: ExecutionState): ExecutionState = {
        state.copy(lexicalFrame = ObjLocation(loc) :: state.lexicalFrame)
    }
}
