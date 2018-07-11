package inferium.dataflow

import inferium.dataflow.CallableInfo.{Anchor, ReturnHandler}
import inferium.dataflow.calls.CallInstance
import inferium.dataflow.graph.{CallNode, MergeNode, Node}
import inferium.lattice.{Entity, Heap}

abstract class CallableInfo {
    def name: Option[String]
    def anchor: Anchor
    def yieldsGraph: Boolean

    def asAnalysable: Analysable
    def instantiate(onReturn: ReturnHandler, priority: Int, catchTarget: Option[MergeNode], callSiteFrame: Node.CallFrame): CallInstance

    override def hashCode(): Int = anchor.hashCode()

    override def equals(other: scala.Any): Boolean = other match {
        case other: CallableInfo => other.anchor == anchor
        case _ => false
    }
}

object CallableInfo {
    type Anchor = AnyRef
    type ReturnHandler = (Entity, Heap, DataFlowAnalysis) => Unit
}
