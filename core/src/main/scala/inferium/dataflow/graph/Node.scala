package inferium.dataflow.graph

import inferium.dataflow.ExecutionContext

abstract class Node {
    def onControlFlow(cdx: ExecutionContext): Unit
}
