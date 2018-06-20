package inferium.dataflow.graph

import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapWriting, LexicalLookup}
import inferium.dataflow.{DataFlowAnalysis, ExecutionState, LexicalEnv}
import inferium.lattice.{StringLattice, ValueLocation}

class LexicalWriteNode(val varName: String)(implicit _info: Node.Info) extends FailingTransformerNode with HeapWriting with LexicalLookup {

    private lazy val lookupChain = info.lexicalEnv.buildLookupSeq(varName)

    override def additionalInfos: Seq[Node.AdditionalInfo] = Node.AdditionalInfo(s"Lookup[$varName]", LexicalEnv.lookupChainToString(lookupChain)) :: Nil

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {

        val writeValue :: restStack = state.stack
        val (obj, propertyName, stateAfterLookup) = lookup(state.copy(stack = restStack), lookupChain)
        write(obj, StringLattice(propertyName), writeValue, stateAfterLookup) map {
            case (result, resultState) => resultState.copy(stack = result :: resultState.stack)
        }
    }

    override def asAsmStmt: String = s"writeL $varName"
}
