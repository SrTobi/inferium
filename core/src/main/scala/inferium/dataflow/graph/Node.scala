package inferium.dataflow.graph

import inferium.dataflow.ExecutionState

abstract class Node {
    private var hasIncomingNode = false
    def addIncomingNode(node: Node): Unit = {
        assert(!hasIncomingNode, "Only one incoming allowed on a normal node")
        hasIncomingNode = true
    }

    def onControlFlow(state: ExecutionState): Unit
    def onNoControlFlow(): Unit
}
