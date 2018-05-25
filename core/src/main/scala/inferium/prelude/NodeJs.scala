package inferium.prelude

import inferium.dataflow.{ExecutionState, LexicalFrame}
import inferium.lattice.{Location, ObjectLike, UndefinedValue}
import inferium.lattice.heaps.SimpleHeap

object NodeJs {
    def initialState: ExecutionState = {
        val (heap, globalObj) = {
            val initialHeap = new SimpleHeap()
            val mutator = initialHeap.begin(Location())

            val gObj = mutator.allocOrdinaryObject(Location())

            (initialHeap.end(mutator), gObj)
        }


        val callFrame = ExecutionState.CallFrame(globalObj, LexicalFrame(globalObj), None)
        new ExecutionState(UndefinedValue :: Nil, heap, callFrame)
    }
}
