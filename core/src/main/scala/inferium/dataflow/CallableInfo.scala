package inferium.dataflow

import inferium.dataflow.calls.CallInstance
import inferium.dataflow.graph.{MergeNode, Node}

abstract class CallableInfo {
    def name: Option[String]

    def instantiate(returnMerger: MergeNode, priority: Int, catchTarget: Option[MergeNode], callFrame: Node.CallFrame): CallInstance
}
