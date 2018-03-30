package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

import scala.collection.mutable

class MergeNode(removable: Boolean = false) extends SingleSuccessor {
    private val preds = mutable.Set.empty[Node]

    override def hasPred: Boolean = preds.nonEmpty

    override def predecessors: Traversable[Node] = preds

    override def setNewInState(state: ExecutionState): Unit = ???

    override def process(): Unit = ???


    override def addPredecessor(node: Node): Unit = {
        assert(node != null)
        preds += node
    }

    override protected def removePredecessor(node: Node): Unit = {
        assert(preds contains node)
        preds -= node
    }
}
