package inferium.dataflow.calls

import inferium.dataflow.graph.Node
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.Entity

abstract class CallInstance {
    def info: CallInstance.Info

    def call(state: ExecutionState, arguments: Seq[Entity], mergedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit
}

object CallInstance {
    sealed abstract class Info
    final case class InlinedCallInfo(entryNode: Node) extends Info
    final case class NativeCallInfo(subcalls: Seq[Subcall]) extends Info
    final case class SignatureCallInfo() extends Info

    case class Subcall()
}
/*
final case class InlinedCallInstance(entryNode: Node) extends CallInstance
final case class NativeCallInstance() extends CallInstance
final case class SignatureCallInstance() extends CallInstance
*/