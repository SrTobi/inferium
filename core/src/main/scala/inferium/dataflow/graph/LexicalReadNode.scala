package inferium.dataflow.graph

import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapReading, LexicalLookup}
import inferium.dataflow.{DataFlowAnalysis, ExecutionState, LexicalEnv}
import inferium.lattice.{StringLattice, ValueLocation}

class LexicalReadNode(val varName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapReading with LexicalLookup {

    private lazy val lookupChain = info.lexicalEnv.buildLookupSeq(varName)

    override def additionalInfos: Seq[Node.AdditionalInfo] = Node.AdditionalInfo(s"Lookup[$varName]", LexicalEnv.lookupChainToString(lookupChain)) :: Nil

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {

        val (obj, propertyName, stateAfterLookup) = lookup(state, lookupChain)
        for((result, stateAfterRead) <- read(obj, StringLattice(propertyName), stateAfterLookup))
            yield stateAfterRead.copy(stack = result :: stateAfterRead.stack)
    }

    override def asAsmStmt: String = s"readL $varName"
}
