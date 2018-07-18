package inferium.prelude

import inferium.Config
import inferium.dataflow.{ExecutionState, LexicalFrame}
import inferium.js.types.js
import inferium.js.types.js.Prelude
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice._
import inferium.lattice.heaps.{ChainHeap, SimpleHeap}
import inferium.prelude.data.NodeJsPreludeData

import scala.collection.mutable

object NodeJs {
    def initialHeap(config: Config, heapFactory: Heap.Factory = SimpleHeap, addPrelude: Boolean = false): (Heap, ObjectLike) = {
        val (initialHeap, specialObjects) = heapFactory.create(config)
        val mutator = initialHeap.begin(Location())

        val gObj = if (addPrelude) {
            {
                val `{}` = mutator.allocOrdinaryObject(Location(), NullValue)
                specialObjects += SpecialObjects.Object -> `{}`
            }
            {
                val Function = mutator.allocOrdinaryObject(Location())
                specialObjects += SpecialObjects.Function -> Function
            }
            val Prelude(gObjType, modules) = js.Prelude.load(NodeJsPreludeData.json)

            gObjType.instantiate(new js.LocGen, mutator, Map.empty, NeverValue, mutable.Map.empty).asInstanceOf[ObjectLike]
        } else {
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
            gObj
        }

        (initialHeap.end(mutator), gObj)
    }
    def initialState(config: Config, heapFactory: Heap.Factory = SimpleHeap, addPrelude: Boolean = false): ExecutionState = {
        val (heap, globalObj) = initialHeap(config, heapFactory, addPrelude)
        new ExecutionState(UndefinedValue :: Nil, heap, globalObj, LexicalFrame(globalObj))
    }
}
