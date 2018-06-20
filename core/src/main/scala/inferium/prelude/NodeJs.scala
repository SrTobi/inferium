package inferium.prelude

import inferium.Config
import inferium.dataflow.{ExecutionState, LexicalFrame}
import inferium.lattice.{Location, ObjectLike, UndefinedValue, ValueLocation}
import inferium.lattice.heaps.SimpleHeap

object NodeJs {
    def initialState(config: Config): ExecutionState = {
        val (heap, globalObj) = {
            val initialHeap = new SimpleHeap(config)
            val mutator = initialHeap.begin(Location())

            val gObj = mutator.allocOrdinaryObject(Location())

            mutator.forceSetPropertyValue(gObj, "global", ValueLocation(Location()), gObj)

            (initialHeap.end(mutator), gObj)
        }

        new ExecutionState(UndefinedValue :: Nil, heap, globalObj, LexicalFrame(globalObj))
    }
}
