package inferium.dataflow.graph

import inferium.dataflow.DataFlowAnalysis
import inferium.dataflow.calls.CallInstance
import inferium.dataflow.graph.Node.CatchTarget
import inferium.dataflow.graph.traits._
import inferium.lattice._

class CallNode(val thisIsOnStack: Boolean, spreadArguments: Seq[Boolean])(implicit _info: Node.Info) extends LinearNode with SingleSuccessor {


    private val heapAccessLoc: Location = Location()

    def argumentCount: Int = spreadArguments.length
    def calls: Iterable[CallInstance.Info] = calling.calls

    private object calling extends Calling with Async.CompleteWithSuccessor {
        override def succ: Node = CallNode.this.succ
        override val thisOrigin: Node.StateOrigin = CallNode.this._thisOrigin
        override def basePriority: Int = info.priority
        override def catchTarget: CatchTarget = info.catchTarget
        override def callFrame: Node.CallFrame = info.callFrame
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

        val mutator = initialHeap.begin(heapAccessLoc)
        lazy val heapAfterSetup = initialHeap.end(mutator)
        lazy val stateAfterSetup = inState.copy(stack = restStack, heap = heapAfterSetup)

        // get callables
        val callables = calling.coerceCallables(func, mutator, stateAfterSetup)

        // normalize spread
        val spreadedArguments = calling.spreadArguments(arguments, spreadArguments)

        calling.call(stateAfterSetup, callables, thisObject, spreadedArguments, NeverValue)
    }

    override def asAsmStmt: String = (if (thisIsOnStack) "invoke" else "call") + s" ($argumentCount args)"
}