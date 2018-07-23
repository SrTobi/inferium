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
    def initialHeap(config: Config, heapFactory: Heap.Factory = SimpleHeap, addPrelude: Boolean = false): (Heap, ObjectLike, Map[String, ObjectLike], js.Instantiator) = {
        val (initialHeap, specialObjects) = heapFactory.create(config)
        val mutator = initialHeap.begin(Location())

        val (gObj, modules, instantiator) = if (addPrelude) {
            {
                val `{}` = mutator.allocOrdinaryObject(Location(), NullValue)
                specialObjects += SpecialObjects.Object -> `{}`
            }
            {
                val Function = mutator.allocOrdinaryObject(Location())
                specialObjects += SpecialObjects.Function -> Function
            }
            val Prelude(gObjType, moduleTypes) = js.Prelude.load(NodeJsPreludeData.json)

            val instantiator = new js.Instantiator(Stream.continually(Location()), NeverValue, mutable.Map.empty)
            val gObj = gObjType.instantiate(mutator, instantiator, Map.empty)
            val modules = moduleTypes map { case (key, value) => key -> value.instantiate(mutator, instantiator, Map.empty) }
            (gObj, modules, instantiator)
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
            (gObj, Map.empty[String, ObjectLike], null)
        }

        (initialHeap.end(mutator), gObj, modules, instantiator)
    }

    def initialState(config: Config, heapFactory: Heap.Factory = SimpleHeap, addPrelude: Boolean = false): (ExecutionState, Map[String, ObjectLike], js.Instantiator) = {
        val (heap, globalObj, modules, instantiator) = initialHeap(config, heapFactory, addPrelude)
        val mutator = heap.begin(Location())
        val mainCtx = mutator.allocOrdinaryObject(Location())
        val finalHeap = heap.end(mutator)
        val resultState = new ExecutionState(UndefinedValue :: Nil, finalHeap, globalObj, mainCtx :: LexicalFrame(globalObj))
        (resultState, modules, instantiator)
    }
}
