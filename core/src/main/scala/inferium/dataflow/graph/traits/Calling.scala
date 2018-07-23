package inferium.dataflow.graph.traits

import inferium.dataflow.calls.{CallInstance, RecursiveCallInstance}
import inferium.dataflow.graph.Node.CallFrame
import inferium.dataflow.{DataFlowAnalysis, ExecutionState, LexicalFrame}
import inferium.lattice._

import scala.collection.mutable

trait Calling extends Async[Unit] with Failing {
    protected val callInstances = mutable.Map.empty[Location, CallInstance]
    protected var savedLocalStack: ExecutionState.Stack = _
    protected var savedLocalThisEntity: Entity = _
    protected var savedLocalLexicalFrame: LexicalFrame = _

    protected var savedReturnValue: Entity = _
    protected var savedReturnHeap: Heap = _
    private lazy val returnProbe = new ProbeEntity

    private val heapReadLoc = Location()

    def basePriority: Int
    def callFrame: CallFrame
    def calls: Iterable[CallInstance.Info] = callInstances.values.map(_.info)


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
        //succ.setNewInState(resultState, callOrigin)(analysis)
        complete(Unit, resultState, analysis)
    }

    def call(stateBeforeCall: ExecutionState, callables: Seq[Callable], thisEntity: Entity, arguments: Seq[Entity], mergedRest: Entity, isConstruction: Boolean)(implicit dataFlowAnalysis: DataFlowAnalysis): Unit = {
        // save local frame stuff
        savedLocalStack = stateBeforeCall.stack
        savedLocalThisEntity = stateBeforeCall.thisEntity
        savedLocalLexicalFrame = stateBeforeCall.lexicalFrame

        val accessor = stateBeforeCall.heap.begin(heapReadLoc)
        lazy val normalizedThis = thisEntity.normalized(accessor)
        lazy val normalizedArguments = arguments.map { _.normalized(accessor) }
        // make the calls
        callables foreach {
            case func@FunctionEntity(loc, frame) =>
                val call = callInstances.getOrElseUpdate(loc, {
                    val callable = func.callableInfo

                    callFrame.getRecursiveSite(callable.anchor) match {
                        case Some(inst) =>
                            new RecursiveCallInstance(inst, ret)
                        case None =>
                            callable.instantiate(ret, basePriority + 1, catchTarget, callFrame, isConstruction)
                    }
                })

                call.call(stateBeforeCall.heap, thisEntity, frame, arguments, mergedRest)

            case probe: ProbeEntity =>
                if (isConstruction) {
                    probe.construct(normalizedArguments, returnProbe)
                } else {
                    probe.call(normalizedThis, normalizedArguments, returnProbe)
                }
                ret(returnProbe, stateBeforeCall.heap, dataFlowAnalysis)
        }
    }

    def coerceCallables(func: Entity, mutator: Heap.Mutator, failState: => ExecutionState)(implicit dataFlowAnalysis: DataFlowAnalysis): Seq[Callable] = {
        var coercingFail = false
        val callables = func.coerceToCallables(mutator, () => {
            coercingFail = true
        })

        if (coercingFail) {
            // todo: fail with better exception
            fail(failState, AnyEntity)
        }

        callables
    }
}
