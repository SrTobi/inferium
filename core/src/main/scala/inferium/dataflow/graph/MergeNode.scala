package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

import scala.collection.mutable

class MergeNode(val fixpoint: Boolean = false, val removable: Boolean = false)(implicit _info: Node.Info) extends Node with SingleSuccessor {
    private val preds = mutable.Set.empty[Node]
    override def hasPred: Boolean = preds.nonEmpty
    override def predecessors: Traversable[Node] = preds

    private var processed: Boolean = true
    private var mergeState: ExecutionState = _

    override def setNewInState(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Unit = {
        val fixedState = fixLexicalFrame(state)

        if (mergeState == null) {
            mergeState = fixedState
        } else {
            // merge
            mergeState = mergeState.merge(Seq(fixedState))
        }

        if (processed) {
            processed = false
            analysis.enqueue(this)
        }
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        processed = true
        succ <~ mergeState
    }


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
