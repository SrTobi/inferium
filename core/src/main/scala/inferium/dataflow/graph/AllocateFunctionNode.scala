package inferium.dataflow.graph

import inferium.dataflow.{CallableInfo, DataFlowAnalysis, ExecutionState, Templates}
import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapWriting, TransformerNode}
import inferium.lattice.{FunctionEntity, Location, StringLattice}
import inferium.lattice.Heap.SpecialObjects

class AllocateFunctionNode(val function: CallableInfo, val captureThis: Boolean)(implicit _info: Node.Info) extends FailingTransformerNode with HeapWriting {
    def name: String = function.name.getOrElse("<anonym>")

    private val heapAccessLoc: Location = Location()
    private val allocSite: Location = Location()
    private val prototypeObjectAllocSite: Location = Location()

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {
        val initialHeap = state.heap

        val mutator = initialHeap.begin(heapAccessLoc)
        val func = mutator.allocObject(allocSite, (l, ac) => {
            FunctionEntity(l, state.lexicalFrame)(ac, function)
        }, initialHeap.specialObject(SpecialObjects.Function))

        val prototypeObject = mutator.allocOrdinaryObject(prototypeObjectAllocSite)

        // todo write this to heap
        val heapAfterAllocation = initialHeap.end(mutator)
        val stateAfterAllocation = state.copy(stack = func :: state.stack, heap = heapAfterAllocation)

        write(func, StringLattice("prototype"), prototypeObject, stateAfterAllocation) map {
            case (_, s) => s
        }
    }

    override def asAsmStmt: String = s"pushFunc $name"
}
