package inferium.dataflow.graph.traits

import inferium.dataflow.{DataFlowAnalysis, ExecutionState, LexicalFrame}
import inferium.dataflow.calls.{CallInstance, RecursiveCallInstance}
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.Node.{CallFrame, CatchTarget, StateOrigin}
import inferium.lattice._

import scala.collection.mutable

trait Calling {
    protected val callInstances = mutable.Map.empty[Location, CallInstance]
    protected var savedLocalStack: ExecutionState.Stack = _
    protected var savedLocalThisEntity: Entity = _
    protected var savedLocalLexicalFrame: LexicalFrame = _

    protected var savedReturnValue: Entity = _
    protected var savedReturnHeap: Heap = _

    def succ: Node
    def callOrigin: StateOrigin
    def basePriority: Int
    def catchTarget: CatchTarget
    def callFrame: CallFrame

    def calls: Iterable[CallInstance.Info] = callInstances.values.map(_.info)

    private def fail(state: ExecutionState, exception: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
        Node.fail(catchTarget, state, exception)(callOrigin, analysis)
    }

    protected def ret(result: Entity, heap: Heap, analysis: DataFlowAnalysis): Unit = {
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
        succ.setNewInState(resultState, callOrigin)(analysis)
    }

    def call(stateBeforeCall: ExecutionState, callables: Seq[Entity], thisEntity: Entity, arguments: Seq[Entity], mergedRest: Entity)(implicit dataFlowAnalysis: DataFlowAnalysis): Unit = {
        // save local frame stuff
        savedLocalStack = stateBeforeCall.stack
        savedLocalThisEntity = stateBeforeCall.thisEntity
        savedLocalLexicalFrame = stateBeforeCall.lexicalFrame

        // make the calls
        for (func@FunctionEntity(loc, frame) <- callables) {
            val call = callInstances.getOrElseUpdate(loc, {
                val callable = func.callableInfo

                callFrame.getRecursiveSite(callable.anchor) match {
                    case Some(inst) =>
                        new RecursiveCallInstance(inst, ret)
                    case None =>
                        callable.instantiate(ret, basePriority + 1, catchTarget, callFrame)
                }
            })

            call.call(stateBeforeCall.heap, thisEntity, frame, arguments, mergedRest)
        }
    }

    def coerceCallables(func: Entity, mutator: Heap.Mutator, failState: => ExecutionState)(implicit dataFlowAnalysis: DataFlowAnalysis): Seq[FunctionEntity] = {
        var coercingFail = false
        val callables = func.coerceToFunctions(mutator, () => {
            coercingFail = true
        })

        if (coercingFail) {
            fail(failState, ???)
        }

        callables
    }

    def spreadArguments(arguments: Seq[Entity], spreadArguments: Seq[Boolean]): Seq[Entity] = {
        arguments zip spreadArguments flatMap {
            case (arg, isSpread) =>
                if (isSpread) {
                    ???
                } else {
                    Seq(arg)
                }
        }
    }
}
