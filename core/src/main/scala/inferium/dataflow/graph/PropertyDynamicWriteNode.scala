package inferium.dataflow.graph
import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapWriting}
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.Location

class PropertyDynamicWriteNode()(implicit _info: Node.Info) extends FailingTransformerNode with HeapWriting {
    private val heapAccessLoc: Location = Location()

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {
        val heap = state.heap
        val writeValue :: property :: base :: rest = state.stack

        val mutator = heap.begin(heapAccessLoc)
        val propertyNames = property.asStringLattice(mutator)
        val heapAfterToStringLattice = heap.end(mutator)

        write(base, propertyNames, writeValue, state.copy(heap = heapAfterToStringLattice, stack = rest)) map {
            case (result, resultState) => resultState.copy(stack = result :: resultState.stack)
        }
    }

    override def asAsmStmt: String = s"writeP <dynamic>"
}
