package inferium.dataflow.graph

import inferium.dataflow.ExecutionState
import inferium.lattice.Entity

abstract class Node {
    final def ~>(node: Node): Graph = {
        this.addSuccessor(node)
        node.addPredecessor(this)
        Graph(this, node)
    }

    final def ~>(graph: Graph): Graph = Graph(this) ~> graph

    final def ~/>(node: Node): Unit = {
        node.removePredecessor(this)
        this.removeSuccessor(node)
    }

    final def erase(): Unit = {
        predecessors foreach { _ ~/> this }
        successors foreach { this ~/> _ }
    }

    final def remove(): Unit = {
        val preds = predecessors.toSeq
        val succs = successors.toSeq

        erase()

        for (pred <- preds; succ <- succs) {
            pred ~> succ
        }
    }

    final def replace(nodes: Node*): Unit = {
        val preds = predecessors.toSeq
        val succs = successors.toSeq

        erase()

        for (pred <- preds; node <- nodes) {
            pred ~> node
        }

        for (succ <- succs; node <- nodes) {
            node ~> succ
        }
    }


    final def fail(state: ExecutionState, exception: Entity): Unit = {
        val throwTarget: Node = ???
        throwTarget <~ state.copy(stack = exception :: Nil)
    }

    def hasPred: Boolean
    def hasSucc: Boolean

    def predecessors: Traversable[Node]
    def successors: Traversable[Node]

    protected def removePredecessor(node: Node): Unit
    protected def removeSuccessor(node: Node): Unit
    protected def addPredecessor(node: Node): Unit
    protected def addSuccessor(node: Node): Unit

    final def <~(state: ExecutionState): Unit = setNewInState(state)
    def setNewInState(state: ExecutionState): Unit

    def process(): Unit
}
