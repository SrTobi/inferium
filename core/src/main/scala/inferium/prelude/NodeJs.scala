package inferium.prelude

import inferium.dataflow.{ExecutionState, LexicalFrame}
import inferium.lattice.{Location, ObjectEntity, UndefinedValue}
import inferium.lattice.heaps.SimpleHeap

object NodeJs {
    def initialState: ExecutionState = {
        val (heap, globalObj) = {
            val initialHeap = new SimpleHeap()
            val mutator = initialHeap.begin(Location())

            val gObj = mutator.allocObject(Location())

            (initialHeap.end(mutator), gObj)
        }


        new ExecutionState(UndefinedValue :: Nil, heap, LexicalFrame(globalObj))
    }
}
