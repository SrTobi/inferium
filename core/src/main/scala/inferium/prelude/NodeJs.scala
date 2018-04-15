package inferium.prelude

import inferium.dataflow.{ExecutionState, LexicalFrame}
import inferium.lattice.{Location, ObjectEntity, UndefinedValue}
import inferium.lattice.heaps.SimpleHeap

object NodeJs {
    def initialState: ExecutionState = {
        val globalObj = ObjectEntity.ordinary(Location())
        val heap = {
            val initialHeap = new SimpleHeap()
            val mutator = initialHeap.begin(Location())

            mutator.allocObject(globalObj)

            initialHeap.end(mutator)
        }


        new ExecutionState(UndefinedValue :: Nil, heap, LexicalFrame(globalObj))
    }
}
