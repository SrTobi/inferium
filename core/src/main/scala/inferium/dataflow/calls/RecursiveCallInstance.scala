package inferium.dataflow.calls
import inferium.dataflow.graph.Node
import inferium.dataflow.{CallableInfo, DataFlowAnalysis, ExecutionState, LexicalFrame}
import inferium.lattice.{Entity, Heap}

class RecursiveCallInstance(site: CallInstance.RecursionAble, private val onReturn: CallableInfo.ReturnHandler) extends CallInstance {
    override def info: CallInstance.Info = CallInstance.RecursiveCallInfo()

    override def call(heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame, arguments: Seq[Entity], mergedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
        site.recursiveCall(heap, thisEntity, lexicalFrame, arguments, mergedRest, this)
    }

    final def ret(result: Entity, heap: Heap, analysis: DataFlowAnalysis): Unit = {
        onReturn(result, heap, analysis)
    }
}
