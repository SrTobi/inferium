package inferium.dataflow.calls

import inferium.dataflow.{DataFlowAnalysis, ExecutionState, LexicalFrame}
import inferium.dataflow.graph.Node
import inferium.lattice._

abstract class InlinedCallInstance extends CallInstance.RecursionAble {
    def entryNode: Node
    private val lengthWriteLoc = ValueLocation(Location())
    private val argumentWriteLocations = Stream.continually(ValueLocation(Location()))
    private val argumentsBuildingLoc = Location()
    private val argumentsObjectLoc = Location()

    override def info: CallInstance.Info = CallInstance.InlinedCallInfo(entryNode)

    protected override def doCall(heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame, arguments: Seq[Entity], unifiedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
        // build arguments object
        val mutator = heap.begin(argumentsBuildingLoc)
        val argumentObject = mutator.allocOrdinaryObject(argumentsObjectLoc)

        // todo: implement variadic arguments because of spreading
        assert(unifiedRest == NeverValue)

        for (((arg, writeLoc), idx) <- (arguments zip argumentWriteLocations).zipWithIndex) {

            mutator.forceSetPropertyValue(argumentObject, idx.toString, writeLoc, arg)
        }

        mutator.forceSetPropertyValue(argumentObject, "length", lengthWriteLoc, SpecificNumberValue(arguments.length))

        val afterHeap = heap.end(mutator)

        // build execution state
        //val afterState = state.copy(stack = argumentObject :: state.stack, heap = afterHeap)
        val callState = ExecutionState(argumentObject :: Nil, afterHeap, thisEntity, lexicalFrame)

        // call entry node
        entryNode <~ callState
    }
}
