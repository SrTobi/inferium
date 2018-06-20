package inferium.dataflow.graph
import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapReading}
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.StringLattice

class PropertyDynamicReadNode()(implicit _info: Node.Info) extends FailingTransformerNode with HeapReading {
    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {
        val heap = state.heap
        val property :: base :: rest = state.stack

        val mutator = heap.begin(loc)
        val propertyNames = property.asStringLattice(mutator)
        val heapAfterToStringLattice = heap.end(mutator)

        read(base, propertyNames, state.copy(heap = heapAfterToStringLattice, stack = rest)) map {
            case (result, resultState) =>
                resultState.copy(stack = result :: resultState.stack)
        }
    }

    override def asAsmStmt: String = s"readP <dynamic>"
}
