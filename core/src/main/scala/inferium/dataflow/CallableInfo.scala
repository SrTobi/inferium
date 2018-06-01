package inferium.dataflow

import inferium.dataflow.CallableInfo.ReturnHandler
import inferium.dataflow.calls.CallInstance
import inferium.dataflow.graph.{CallNode, MergeNode, Node}
import inferium.lattice.{Entity, Heap}

abstract class CallableInfo {
    def name: Option[String]
    def anchor: AnyRef

    def instantiate(onReturn: ReturnHandler, priority: Int, catchTarget: Option[MergeNode], callSiteFrame: Node.CallFrame): CallInstance
}

object CallableInfo {
    type ReturnHandler = (Entity, Heap, DataFlowAnalysis) => Unit
}
