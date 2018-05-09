package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

class PropertyWriteNode(propertyName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapWriting {
    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {
        val writeValue :: base :: rest = state.stack

        write(base, propertyName, writeValue, state.copy(stack = rest)) map {
            case (result, resultState) => resultState.copy(stack = result :: resultState.stack)
        }
    }

    override def asAsmStmt: String = s"writeP $propertyName"
}
