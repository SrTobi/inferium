package inferium.dataflow.calls
import inferium.dataflow.CallableInfo.{Anchor, ReturnHandler}
import inferium.dataflow.graph.{MergeNode, Node}
import inferium.dataflow.{Analysable, CallableInfo, DataFlowAnalysis, LexicalFrame}
import inferium.lattice.{BuiltInFunctionEntity, Entity, Heap, Location}

abstract class NativeCallableInfo(val nativeName: String) extends CallableInfo {
    override def name: Option[String] = Some(nativeName)
    override def yieldsGraph: Boolean = false
    override def asAnalysable: Analysable = throw new UnsupportedOperationException("Native call can not be analysed")
}

object NativeCall {
    type SimpleImpl = (Heap, Entity, Seq[Entity], Entity, DataFlowAnalysis) => (Heap, Entity)
    def createSimpleCallableInfo(nativeName: String, impl: SimpleImpl): NativeCallableInfo = new NativeCallableInfo(nativeName) {
        override def anchor: Anchor = impl
        override def instantiate(onReturn: ReturnHandler, priority: Int, catchTarget: Option[MergeNode], callSiteFrame: Node.CallFrame, isConstruction: Boolean): CallInstance = new CallInstance {
            override def info: CallInstance.Info = CallInstance.NativeCallInfo(nativeName, Seq.empty)

            override def call(heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame, arguments: Seq[Entity], mergedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
                val (retHeap, ret) = impl(heap, thisEntity, arguments, mergedRest, analysis)

                onReturn(ret, retHeap, analysis)
            }
        }
    }

    def createSimpleFunction(nativeName: String, impl: SimpleImpl, mutator: Heap.Mutator): BuiltInFunctionEntity = {
        mutator.allocObject(Location(), (loc, ac) => {
            assert(ac == 1)
            new BuiltInFunctionEntity(loc, createSimpleCallableInfo(nativeName, impl))
        }, mutator.specialObject(Heap.SpecialObjects.Function))
    }
}