package inferium.dataflow

import inferium.js.types.js.Instantiator
import inferium.lattice.ObjectLike

abstract class DataFlowAnalysis(val debugAdapter: DebugAdapter) {

    def enqueue(node: graph.Node)
    def instantiator: Instantiator

    def requireModule(name: String): Option[ObjectLike] = None
}
