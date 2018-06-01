package inferium.dataflow.graph

import inferium.dataflow.{DataFlowAnalysis, ExecutionState, LexicalEnv, LexicalFrame}
import inferium.dataflow.calls.{CallInstance, RecursiveCallInstance}
import inferium.dataflow.graph.MergeNode.MergeType
import inferium.dataflow.graph.traits.{HeapWriting, LinearNode, SingleSuccessor}
import inferium.lattice._
import inferium.utils.Utils._

import scala.collection.mutable

class CallNode(val thisIsOnStack: Boolean, spreadArguments: Seq[Boolean])(implicit _info: Node.Info) extends LinearNode with SingleSuccessor with HeapWriting{

    private val callInstances = mutable.Map.empty[Location, CallInstance]
    private var savedLocalStack: ExecutionState.Stack = _
    private var savedLocalThisEntity: Entity = _
    private var savedLocalLexicalFrame: LexicalFrame = _

    private var savedReturnValue: Entity = _
    private var savedReturnHeap: Heap = _

    def argumentCount: Int = spreadArguments.length
    def calls: Iterable[CallInstance.Info] = callInstances.values.map(_.info)

    private def ret(result: Entity, heap: Heap, analysis: DataFlowAnalysis): Unit = {
        if (savedReturnValue == null) {
            assert(savedReturnHeap == null)
            savedReturnValue = result
            savedReturnHeap = heap
        } else {
            assert(savedReturnHeap != null)
            savedReturnValue = savedReturnValue unify result
            savedReturnHeap = savedReturnHeap unify heap
        }

        val resultState = ExecutionState(savedReturnValue :: savedLocalStack, savedReturnHeap, savedLocalThisEntity, savedLocalLexicalFrame)
        implicit val _: DataFlowAnalysis = analysis
        succ <~ resultState
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        val initialHeap = inState.heap
        val initialStack = inState.stack


        // get arguments
        val (reverseArguments, stackWithoutArgs) = initialStack.splitAt(argumentCount)
        val arguments = reverseArguments.reverse
        assert(arguments.length == argumentCount)


        // get function entity
        val func :: stackWithoutFunc = stackWithoutArgs

        // get this
        val (thisObject, restStack) = if (thisIsOnStack) {
            val thisObject :: restStack = stackWithoutFunc
            (thisObject, restStack)
        } else {
            val thisObject = UndefinedValue
            (thisObject, stackWithoutFunc)
        }

        val mutator = initialHeap.begin(loc)
        lazy val heapAfterSetup = initialHeap.end(mutator)



        lazy val stateAfterSetup = inState.copy(stack = restStack, heap = heapAfterSetup)

        // get callables
        var coercingFail = false
        val callables = func.coerceToFunctions(mutator, () => {
                    coercingFail = true
                })

        if (coercingFail) {
            fail(stateAfterSetup, ???)
        }

        // normalize spread
        val spreadedArguments = arguments zip spreadArguments flatMap {
            case (arg, isSpread) =>
                if (isSpread) {
                    ???
                } else {
                    Seq(arg)
                }
        }

        // save local frame stuff
        val stateBeforeCall = stateAfterSetup
        savedLocalStack = stateBeforeCall.stack
        savedLocalThisEntity = stateBeforeCall.thisEntity
        savedLocalLexicalFrame = stateBeforeCall.lexicalFrame

        // make the calls
        for (func@FunctionEntity(loc, frame) <- callables) {
            val call = callInstances.getOrElseUpdate(loc, {
                val callable = func.callableInfo

                info.callFrame.getRecursiveSite(callable.anchor) match {
                    case Some(inst) =>
                        new RecursiveCallInstance(inst, ret)
                    case None =>
                        callable.instantiate(ret, info.priority + 1, info.catchTarget, info.callFrame)
                }
            })

            call.call(stateBeforeCall.heap, stateBeforeCall.thisEntity, frame, spreadedArguments, NeverValue)
        }
    }

    override def asAsmStmt: String = (if (thisIsOnStack) "invoke" else "call") + s" ($argumentCount args)"
}