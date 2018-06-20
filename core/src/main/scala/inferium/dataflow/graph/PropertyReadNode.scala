package inferium.dataflow.graph
import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapReading}
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.StringLattice

class PropertyReadNode(val propertyName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapReading {
    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {
        val base :: rest = state.stack

        read(base, StringLattice(propertyName), state.copy(stack = rest)) map {
            case (result, resultState) =>
                resultState.copy(stack = result :: resultState.stack)
        }
    }

    override def asAsmStmt: String = s"readP $propertyName"
}
