package inferium.prelude

import inferium.Config
import inferium.dataflow.{ExecutionState, LexicalFrame}
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice._
import inferium.lattice.heaps.{ChainHeap, SimpleHeap}

object NodeJs {
    def initialHeap(config: Config, heapFactory: Heap.Factory = SimpleHeap): (Heap, ObjectLike) = {
        val (initialHeap, specialObjects) = heapFactory.create(config)
        val mutator = initialHeap.begin(Location())

        {
            val `{}` = mutator.allocOrdinaryObject(Location(), NullValue)
            specialObjects += SpecialObjects.Object -> `{}`
        }
        {
            val Function = mutator.allocOrdinaryObject(Location())
            specialObjects += SpecialObjects.Function -> Function
        }
        {
            val Array = mutator.allocOrdinaryObject(Location())
            specialObjects += SpecialObjects.Array -> Array
        }

        val gObj = mutator.allocOrdinaryObject(Location())

        mutator.forceSetPropertyValue(gObj, "global", Location(), gObj)

        (initialHeap.end(mutator), gObj)
    }
    def initialState(config: Config, heapFactory: Heap.Factory = SimpleHeap): ExecutionState = {
        val (heap, globalObj) = initialHeap(config, heapFactory)
        new ExecutionState(UndefinedValue :: Nil, heap, globalObj, LexicalFrame(globalObj))
    }
}
