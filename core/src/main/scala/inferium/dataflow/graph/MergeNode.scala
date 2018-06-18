package inferium.dataflow.graph
import inferium.Unifiable
import inferium.dataflow.graph.MergeNode.MergeType
import inferium.dataflow.graph.MergeNode.MergeType.MergeType
import inferium.dataflow.graph.traits.SingleSuccessor
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.Location

import scala.collection.mutable

class MergeNode(val mergeType: MergeType = MergeType.Normal, val removable: Boolean = false)(implicit _info: Node.Info) extends Node with SingleSuccessor {
    private val preds = mutable.Buffer.empty[Node]
    override def hasPred: Boolean = preds.nonEmpty
    override def predecessors: Seq[Node] = preds

    def isFixpoint: Boolean = mergeType == MergeType.Fixpoint
    def isCatchMerger: Boolean = mergeType == MergeType.CatchMerger
    private var processed: Boolean = true
    private var mergeState: ExecutionState = _

    override def setNewInState(state: ExecutionState, origin: Location)(implicit analysis: DataFlowAnalysis): Unit = {
        implicit val useFixpoint: Unifiable.Fixpoint = new Unifiable.Fixpoint(isFixpoint)
        val fixedState = fixLexicalFrame(state)

        if (mergeState == null) {
            mergeState = fixedState
        } else {
            // merge
            val newMergeState = mergeState unify fixedState

            if (mergeState == newMergeState) {
                return
            }

            mergeState = newMergeState
        }

        if (processed) {
            processed = false
            analysis.enqueue(this)
        }
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        processed = true
        succ <~ mergeState
        //if (!isFixpoint)
        //    mergeState = null
    }


    protected[graph] override def addPredecessor(node: Node): Unit = {
        assert(node != null)
        if (!preds.contains(node))
            preds += node
    }

    protected[graph] override def removePredecessor(node: Node): Unit = {
        assert(preds contains node)
        preds -= node
    }

    override def asAsmStmt: String = {
        val prefix = mergeType match {
            case MergeType.Normal => ""
            case MergeType.Fixpoint => "fixpoint-"
            case MergeType.CallMerger => "call-"
            case MergeType.CatchMerger => "catch-"
        }
        s"${prefix}merge[${preds.size} nodes]"
    }
}

object MergeNode {
    object MergeType extends Enumeration {
        type MergeType = Value
        val Normal, Fixpoint, CatchMerger, CallMerger = Value
    }
}