package inferium.dataflow.graph

import inferium.dataflow.ExecutionState
import inferium.lattice.ValueLocation

class LexicalReadNode(val varName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapReading with LexicalLookup {

    override def lookupName: String = varName

    override protected def transform(state: ExecutionState): Option[ExecutionState] = {

        val (obj, propertyName, stateAfterLookup) = lookup(state)
        for((result, stateAfterRead) <- read(Seq(obj), ValueLocation.Scope, propertyName, stateAfterLookup))
            yield stateAfterRead.copy(stack = result :: stateAfterRead.stack)
    }

    override def asAsmStmt: String = s"read $varName"
}
