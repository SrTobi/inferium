package inferium.dataflow

import inferium.dataflow.CallableInfo.{AnalysisInfo, Anchor, ReturnHandler}
import inferium.dataflow.calls.CallInstance
import inferium.dataflow.graph.{CallNode, MergeNode, Node}
import inferium.lattice.{Entity, Heap, NeverValue, ProbeEntity}

abstract class CallableInfo {
    def name: Option[String]
    def argumentNames: Array[String] = ('a' to 'z').map { _.toString }.toArray
    def anchor: Anchor
    def yieldsGraph: Boolean

    def asAnalysable: Analysable
    def instantiate(onReturn: ReturnHandler, priority: Int, catchTarget: Option[MergeNode], callSiteFrame: Node.CallFrame, isConstruction: Boolean): CallInstance

    override def hashCode(): Int = anchor.hashCode()

    override def equals(other: scala.Any): Boolean = other match {
        case other: CallableInfo => other.anchor eq anchor
        case _ => false
    }

    var analysisInfo: Option[AnalysisInfo] = None
}

object CallableInfo {
    class AnalysisInfo  {
        val thisProbe = new ProbeEntity
        val argumentProbe = new ProbeEntity
        var returnValue: Entity = NeverValue
    }
    type Anchor = AnyRef
    type ReturnHandler = (Entity, Heap, DataFlowAnalysis) => Unit
}
