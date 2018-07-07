package inferium.dataflow.graph

import inferium.dataflow.calls.CallInstance
import inferium.dataflow.graph.Node.CatchTarget
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.traits._
import inferium.lattice._


class NewNode(spreadArguments: Seq[Boolean])(implicit _info: Node.Info) extends LinearNode with SingleSuccessor with HeapReading {
    private val heapAccessLoc: Location = Location()
    private val allocSite: Location = Location()


    def argumentCount: Int = spreadArguments.length
    def calls: Iterable[CallInstance.Info] = calling.calls

    private object calling extends Calling with Async.CompleteWithSuccessor {
        override def succ: Node = NewNode.this.succ
        override val thisOrigin: Node.StateOrigin = _thisOrigin
        override def basePriority: Int = info.priority
        override def catchTarget: CatchTarget = info.catchTarget
        override def callFrame: Node.CallFrame = info.callFrame

        private val heapAccessLoc: Location = Location()

        override protected def ret(result: Entity, heap: Heap, analysis: DataFlowAnalysis): Unit = {
            if (savedReturnHeap == null) {
                savedReturnValue = result
                savedReturnHeap = heap
            } else {
                assert(savedReturnHeap != null)
                savedReturnValue = savedReturnValue unify result
                savedReturnHeap = savedReturnHeap unify heap
            }

            // we know, that the top of the stack must be the new object
            val (newObj: ObjectLike) :: localStack = savedLocalStack
            val constructionResult = Entity.unify(savedReturnValue.coerceToConstructionObject(heap.begin(heapAccessLoc), newObj))

            val resultState = ExecutionState(constructionResult :: localStack, savedReturnHeap, savedLocalThisEntity, savedLocalLexicalFrame)
            complete(Unit, resultState, analysis)
        }
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = {

        val initialStack = inState.stack


        // get arguments
        val (reverseArguments, stackWithoutArgs) = initialStack.splitAt(argumentCount)
        val arguments = reverseArguments.reverse
        assert(arguments.length == argumentCount)


        // get function entity
        val func :: stackWithoutFunc = stackWithoutArgs


        val (prototype, stateBeforeSetup) = read(func, StringLattice("prototype"), inState.copy(stack = stackWithoutFunc)) getOrElse {
            // yes! this works! scala is awesome
            return
        }

        val heapBeforeSetup = stateBeforeSetup.heap
        val mutator = heapBeforeSetup.begin(heapAccessLoc)

        val newObj = mutator.allocOrdinaryObject(allocSite, prototype)

        lazy val heapAfterSetup = heapBeforeSetup.end(mutator)
        lazy val stateAfterSetup = stateBeforeSetup.copy(heap = heapAfterSetup, stack = newObj :: stateBeforeSetup.stack)


        // get callables
        val callables = calling.coerceCallables(func, mutator, stateAfterSetup)

        // normalize spread
        val spreadedArguments = calling.spreadArguments(arguments, spreadArguments)

        calling.call(stateAfterSetup, callables, newObj, spreadedArguments, NeverValue)
    }

    override def asAsmStmt: String = s"new ($argumentCount args)"
}
