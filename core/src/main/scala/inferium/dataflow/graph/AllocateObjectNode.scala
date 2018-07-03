package inferium.dataflow.graph
import inferium.dataflow.graph.traits.TransformerNode
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.{Location, ObjectLike, ObjectType}

class AllocateObjectNode(implicit _info: Node.Info) extends TransformerNode {
    private val heapAccessLoc: Location = Location()
    private val allocSite: Location = Location()

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val initialHeap = state.heap

        val mutator = initialHeap.begin(heapAccessLoc)
        val obj = mutator.allocOrdinaryObject(allocSite)
        val resultHeap = initialHeap.end(mutator)

        state.copy(stack = obj :: state.stack, heap = resultHeap)
    }

    override def asAsmStmt: String = "allocObj"
}
