package inferium.dataflow.graph

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.ValueLocation

class LexicalWriteNode(val varName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapWriting with LexicalLookup {

    private lazy val lookupChain = info.lexicalEnv.buildLookupSeq(varName)

    override protected def transform(state: ExecutionState, analysis: DataFlowAnalysis): Option[ExecutionState] = {

        val writeValue :: restStack = state.stack
        val (obj, propertyName, stateAfterLookup) = lookup(state.copy(stack = restStack), lookupChain)
        write(Seq(obj), ValueLocation.Scope, propertyName, writeValue, stateAfterLookup)
    }

    override def asAsmStmt: String = s"write $varName"
}
