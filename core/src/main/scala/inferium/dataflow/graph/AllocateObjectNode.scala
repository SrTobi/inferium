package inferium.dataflow.graph
import inferium.dataflow.graph.traits.TransformerNode
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.{ObjectEntity, ObjectType}

class AllocateObjectNode(implicit _info: Node.Info) extends TransformerNode {
    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val initialHeap = state.heap

        val mutator = initialHeap.begin(loc)
        val obj = mutator.allocObject(loc)
        val resultHeap = initialHeap.end(mutator)

        state.copy(stack = obj :: state.stack, heap = resultHeap)
    }

    override def asAsmStmt: String = "allocObj"
}
