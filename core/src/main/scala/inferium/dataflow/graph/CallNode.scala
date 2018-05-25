package inferium.dataflow.graph

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.calls.CallInstance
import inferium.dataflow.graph.MergeNode.MergeType
import inferium.dataflow.graph.traits.{HeapWriting, LinearNode, SingleSuccessor}
import inferium.lattice._

import scala.collection.mutable

class CallNode(val thisIsOnStack: Boolean, spreadArguments: Seq[Boolean])(implicit _info: Node.Info) extends LinearNode with SingleSuccessor with HeapWriting{

    private val callInstances = mutable.Map.empty[Location, CallInstance]
    private val returnMergeNode = new MergeNode(MergeType.CallMerger)(info.copy(priority = info.priority + 2, label = None))

    def argumentCount: Int = spreadArguments.length
    def calls: Iterable[CallInstance.Info] = callInstances.values.map(_.info)

    protected[graph] override def addSuccessor(node: Node): Unit = {
        super.addSuccessor(node)
        returnMergeNode.addSuccessor(node)
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
            (thisObject, initialStack)
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

        // make the calls
        val stateBeforeCall = inState.copy(stack = restStack, heap = heapAfterSetup)
        for (func@FunctionEntity(loc, frame) <- callables) {
            val call = callInstances.getOrElseUpdate(loc, {
                val callable = func.callableInfo
                val newFrame = this :: info.callFrame
                callable.instantiate(returnMergeNode, info.priority + 1, info.catchTarget, newFrame)
            })
            val newCallFrame = ExecutionState.CallFrame(thisObject, frame, Some(stateBeforeCall.callFrame))
            val callState = stateBeforeCall.withCallFrame(newCallFrame)

            call.call(callState, spreadedArguments, NeverValue)
        }

    }

    override def asAsmStmt: String = "call"
}