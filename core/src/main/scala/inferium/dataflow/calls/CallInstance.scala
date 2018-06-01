package inferium.dataflow.calls

import inferium.dataflow.graph.Node
import inferium.dataflow.{CallableInfo, DataFlowAnalysis, ExecutionState, LexicalFrame}
import inferium.lattice.{Entity, Heap, NeverValue, UndefinedValue}

import scala.collection.mutable

abstract class CallInstance {
    def info: CallInstance.Info

    def call(heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame, arguments: Seq[Entity], mergedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit
}

object CallInstance {
    sealed abstract class Info
    final case class InlinedCallInfo(entryNode: Node) extends Info
    final case class NativeCallInfo(subcalls: Seq[Subcall]) extends Info
    final case class SignatureCallInfo() extends Info
    final case class RecursiveCallInfo() extends Info

    case class Subcall()

    abstract class RecursionAble extends CallInstance {
        private var isRecursive = false
        private var hasReturned = false
        private var invocationHeap: Heap = _
        private var invocationThisEntity: Entity = _
        private var invocationLexFrames: LexicalFrame = _
        private var invocationArguments: Seq[Entity] = Seq()
        private var invocationMergedRest: Entity = NeverValue
        private var returnHeap: Heap = _
        private var returnEntity: Entity = _
        private val recursiveCallSites = mutable.Set.empty[RecursiveCallInstance]

        def info: CallInstance.Info

        protected def onReturn: CallableInfo.ReturnHandler

        final override def call(heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame, arguments: Seq[Entity], mergedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
            if (!isRecursive) {
                invocationHeap = heap
                invocationThisEntity = thisEntity
                invocationLexFrames = lexicalFrame
                invocationArguments = arguments
                invocationMergedRest = mergedRest
            } else {
                assert(invocationHeap != null && invocationThisEntity != null && invocationLexFrames != null && invocationArguments != null && invocationMergedRest != null)
                invocationHeap = invocationHeap fixpointUnify heap
                invocationThisEntity = invocationThisEntity unify thisEntity
                invocationLexFrames = invocationLexFrames unify lexicalFrame
                invocationArguments = invocationArguments.zipAll(arguments, UndefinedValue, UndefinedValue).map { case (x, y) => x unify y }
                invocationMergedRest = invocationMergedRest unify mergedRest
            }

            doCall(invocationHeap,invocationThisEntity, invocationLexFrames, invocationArguments, invocationMergedRest)
        }

        final def recursiveCall(heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame, arguments: Seq[Entity], mergedRest: Entity, callSite: RecursiveCallInstance)(implicit analysis: DataFlowAnalysis): Unit = {
            isRecursive = true
            recursiveCallSites += callSite
            call(heap, thisEntity, lexicalFrame, arguments, mergedRest)
        }

        protected def doCall(heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame, arguments: Seq[Entity], mergedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit

        final def ret(result: Entity, heap: Heap, analysis: DataFlowAnalysis): Unit = {
            if (!isRecursive || !hasReturned) {
                returnEntity = result
                returnHeap = heap
            } else {
                assert(returnEntity != null && returnHeap != null)
                returnEntity = returnEntity unify result
                returnHeap = returnHeap fixpointUnify heap
            }
            hasReturned = true
            recursiveCallSites foreach { _.ret(returnEntity, returnHeap, analysis) }
            onReturn(returnEntity, returnHeap, analysis)
        }
    }
}
/*
final case class InlinedCallInstance(entryNode: Node) extends CallInstance
final case class NativeCallInstance() extends CallInstance
final case class SignatureCallInstance() extends CallInstance
*/