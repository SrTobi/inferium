package inferium.dataflow

import inferium.js.types.js.Instantiator
import inferium.lattice.{Entity, Heap, ObjectLike}

abstract class DataFlowAnalysis(val debugAdapter: DebugAdapter) {

    def enqueue(node: graph.Node)
    def instantiator: Instantiator

    def requireModule(name: String, heap: Heap): Option[(Entity, Heap)] = None
}
