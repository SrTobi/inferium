package inferium.dataflow.graph

import inferium.dataflow.{CallableInfo, DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapWriting}
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice.{FunctionEntity, Location, StringLattice}

class AllocateArrayNode(val spreadElements: Seq[Boolean])(implicit _info: Node.Info) extends FailingTransformerNode with HeapWriting {

    private val heapAccessLoc: Location = Location()
    private val allocSite: Location = Location()

    val elements: Int = spreadElements.length

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {
        val initialHeap = state.heap

        val mutator = initialHeap.begin(heapAccessLoc)
        val func = mutator.allocOrdinaryObject(allocSite, initialHeap.specialObject(SpecialObjects.Array))


    }

    override def asAsmStmt: String = s"pushArray $elements"
}
