package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

class MergeNode(removable: Boolean = false) extends LinearNode {
    var incomingCount: Int = 0

    override def addIncomingNode(node: Node): Unit = {
        incomingCount += 1
    }

    override def onControlFlow(state: ExecutionState): Unit = ???
    override def onNoControlFlow(): Unit = ???
}
