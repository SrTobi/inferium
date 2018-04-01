package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

import scala.collection.mutable

class MergeNode(val fixpoint: Boolean = false, val removable: Boolean = false)(implicit _info: Node.Info) extends Node with SingleSuccessor {
    private val preds = mutable.Set.empty[Node]

    override def hasPred: Boolean = preds.nonEmpty

    override def predecessors: Traversable[Node] = preds

    override def setNewInState(state: ExecutionState): Unit = ???

    override def process(): Unit = ???


    protected[graph] override def addPredecessor(node: Node): Unit = {
        assert(node != null)
        preds += node
    }

    protected[graph] override def removePredecessor(node: Node): Unit = {
        assert(preds contains node)
        preds -= node
    }

    override def toString: String = s"${if (fixpoint) "fixpoint-" else ""}merge[${preds.size} nodes]"
}
