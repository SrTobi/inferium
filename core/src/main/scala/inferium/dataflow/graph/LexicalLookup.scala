package inferium.dataflow.graph

import inferium.dataflow.ExecutionState
import inferium.dataflow.LexicalEnv.{LookupItem, LookupType}
import inferium.lattice.ObjectEntity

trait LexicalLookup extends Node{
    def lookupName: String
    private lazy val lookupChain = info.lexicalEnv.buildLookupSeq(lookupName)

    def lookup(state: ExecutionState): (ObjectEntity, String, ExecutionState) = {
        val lexObjs = state.lexicalFrame.objects

        lookupChain.view.flatMap {
            case LookupItem(LookupType.Declarative, property, objIdx) =>
                // we know, that we can find the property on the referenced lexical object. Get it!
                val obj = lexObjs(objIdx)
                Some((obj, property, state))

            case LookupItem(LookupType.Computed, property, objIdx) =>
                // lookup in object and if it has the property return it
                ???

            case LookupItem(LookupType.Global, property, objIdx) =>
                // lookup in global object
                ???
        }.head
    }
}
