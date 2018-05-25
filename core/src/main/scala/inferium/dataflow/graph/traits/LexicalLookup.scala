package inferium.dataflow.graph.traits

import inferium.dataflow.{ExecutionState, LexicalEnv}
import inferium.dataflow.LexicalEnv.{LookupItem, LookupType}
import inferium.dataflow.graph.Node
import inferium.lattice.{Entity, ObjectLike}

trait LexicalLookup extends Node{

    def lookup(state: ExecutionState, lookupChain: List[LookupItem]): (Entity, String, ExecutionState) = {
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
                val obj = lexObjs(objIdx)
                Some((obj, property, state))
        }.head
    }

    def lookupChainToString(lookupChain: List[LookupItem]): String = LexicalEnv.lookupChainToString(lookupChain)
}
