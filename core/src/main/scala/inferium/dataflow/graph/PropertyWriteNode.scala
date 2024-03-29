package inferium.dataflow.graph
import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapWriting}
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.StringLattice

class PropertyWriteNode(propertyName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapWriting {
    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {
        val writeValue :: base :: rest = state.stack

        write(base, StringLattice(propertyName), writeValue, state.copy(stack = rest)) map {
            case (result, resultState) => resultState.copy(stack = result :: resultState.stack)
        }
    }

    override def asAsmStmt: String = s"writeP $propertyName"
}
