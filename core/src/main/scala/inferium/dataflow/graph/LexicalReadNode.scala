package inferium.dataflow.graph

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.ValueLocation

class LexicalReadNode(val varName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapReading with LexicalLookup {

    private lazy val lookupChain = info.lexicalEnv.buildLookupSeq(varName)

    override protected def transform(state: ExecutionState, analysis: DataFlowAnalysis): Option[ExecutionState] = {

        val (obj, propertyName, stateAfterLookup) = lookup(state, lookupChain)
        for((result, stateAfterRead) <- read(Seq(obj), ValueLocation.Scope, propertyName, stateAfterLookup))
            yield stateAfterRead.copy(stack = result :: stateAfterRead.stack)
    }

    override def asAsmStmt: String = s"read $varName"
}
