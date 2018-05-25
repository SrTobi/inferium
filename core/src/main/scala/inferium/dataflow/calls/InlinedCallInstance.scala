package inferium.dataflow.calls

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.Node
import inferium.lattice._

class InlinedCallInstance(entryNode: Node) extends CallInstance {
    private val lengthWriteLoc = ValueLocation(Location())
    private val argumentWriteLocations = Stream.continually(ValueLocation(Location()))
    private val argumentsBuildingLoc = Location()
    private val argumentsObjectLoc = Location()

    override def info: CallInstance.Info = CallInstance.InlinedCallInfo(entryNode)

    override def call(state: ExecutionState, arguments: Seq[Entity], unifiedRest: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
        // build arguments object
        val heap = state.heap
        val mutator = heap.begin(argumentsBuildingLoc)
        val argumentObject = mutator.allocOrdinaryObject(argumentsObjectLoc)

        // todo: implement variadic arguments because of spreading
        assert(unifiedRest == NeverValue)

        for (((arg, writeLoc), idx) <- (arguments zip argumentWriteLocations).zipWithIndex) {

            mutator.forceSetPropertyValue(argumentObject, idx.toString, writeLoc, arg)
        }

        mutator.forceSetPropertyValue(argumentObject, "length", lengthWriteLoc, SpecificNumberValue(arguments.length))

        val afterHeap = heap.end(mutator)
        val afterState = state.copy(stack = argumentObject :: state.stack, heap = afterHeap)
        entryNode <~ afterState
    }
}
