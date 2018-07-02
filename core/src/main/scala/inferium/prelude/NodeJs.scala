package inferium.prelude

import inferium.Config
import inferium.dataflow.{ExecutionState, LexicalFrame}
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice._
import inferium.lattice.heaps.SimpleHeap

object NodeJs {
    def initialState(config: Config): ExecutionState = {
        val (heap, globalObj) = {
            val (initialHeap, specialObjects) = SimpleHeap.create(config)
            val mutator = initialHeap.begin(Location())

            {
                val `{}` = mutator.allocOrdinaryObject(Location(), NullValue)
                specialObjects += SpecialObjects.Object -> `{}`
            }
            {
                val Function = mutator.allocOrdinaryObject(Location())
                specialObjects += SpecialObjects.Function -> Function
            }
            val gObj = mutator.allocOrdinaryObject(Location())

            mutator.forceSetPropertyValue(gObj, "global", ValueLocation(Location()), gObj)

            (initialHeap.end(mutator), gObj)
        }

        new ExecutionState(UndefinedValue :: Nil, heap, globalObj, LexicalFrame(globalObj))
    }
}
