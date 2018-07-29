package inferium.dataflow.calls

import inferium.dataflow.CallableInfo.{Anchor, ReturnHandler}
import inferium.dataflow.graph.{MergeNode, Node}
import inferium.dataflow.{Analysable, CallableInfo, DataFlowAnalysis, LexicalFrame}
import inferium.js.types.js
import inferium.js.types.js.Instantiator
import inferium.lattice._

import scala.collection.mutable

abstract class SignatureCall(override val name: Option[String], val callSig: js.Signature, val constructSig: js.Signature) extends CallableInfo {
    override def anchor: Anchor = this

    override def hashCode(): Int = name.hashCode()

    override def yieldsGraph: Boolean = false
    override def asAnalysable: Analysable = throw new UnsupportedOperationException("Signature call can not be analysed")
}

object SignatureCall {
    def createCallableInfo(_name: Option[String], _callSig: js.Signature, _constructSig: js.Signature): CallableInfo = new SignatureCall(_name, _callSig, _constructSig) {
        override def instantiate(onReturn: ReturnHandler, priority: Int, catchTarget: Option[MergeNode], callSiteFrame: Node.CallFrame, isConstruction: Boolean): CallInstance = {
            new CallInstance {
                override def info: CallInstance.Info = CallInstance.SignatureCallInfo(_name.getOrElse("unknown-sig"), Seq.empty)
                private val signature = if (isConstruction) constructSig else callSig
                private val instantiaters = Stream.continually(new Instantiator(Stream.continually(Location()), AnyEntity, mutable.Map.empty))
                private val heapAccess = Location()

                override def call(heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame, arguments: Seq[Entity], mergedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
                    val mutator = heap.begin(heapAccess)
                    val result = Entity.unify((signature zip instantiaters) map {
                        case (overload, instantiator) =>
                            lazy val minNeeded = overload.params.takeWhile(!_.optional).length
                            if (mergedRest == NeverValue && minNeeded > arguments.size) {
                                AnyEntity
                            } else {
                                if ((arguments zip overload.params).forall { case (arg, param) => param.ty.matches(arg)}) {
                                    overload.returnType.instantiate(mutator, instantiator, Map.empty)
                                } else {
                                    AnyEntity
                                }
                            }
                    })
                    val resultHeap = heap.end(mutator)
                    onReturn(result, resultHeap, analysis)
                }
            }
        }
    }
}
