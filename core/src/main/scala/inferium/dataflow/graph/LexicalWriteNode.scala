package inferium.dataflow.graph

import inferium.dataflow.ExecutionState
import inferium.lattice.ValueLocation

class LexicalWriteNode(val varName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapWriting with LexicalLookup {

    override def lookupName: String = varName

    override protected def transform(state: ExecutionState): Option[ExecutionState] = {

        val writeValue :: restStack = state.stack
        val (obj, propertyName, stateAfterLookup) = lookup(state.copy(stack = restStack))
        write(Seq(obj), ValueLocation.Scope, propertyName, writeValue, stateAfterLookup)
    }

    override def asAsmStmt: String = s"write $varName"
}
