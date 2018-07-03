package inferium.dataflow.graph

import inferium.dataflow.{CallableInfo, DataFlowAnalysis, ExecutionState, Templates}
import inferium.dataflow.graph.traits.TransformerNode
import inferium.lattice.{FunctionEntity, Location}
import inferium.lattice.Heap.SpecialObjects

class AllocateFunctionNode(val function: CallableInfo, val captureThis: Boolean)(implicit _info: Node.Info) extends TransformerNode {
    def name: String = function.name.getOrElse("<anonym>")

    private val heapAccessLoc: Location = Location()
    private val allocSite: Location = Location()

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val initialHeap = state.heap

        val mutator = initialHeap.begin(heapAccessLoc)
        val obj = mutator.allocObject(allocSite, (l, ac) => {
            FunctionEntity(l, state.lexicalFrame)(ac, function)
        }, initialHeap.specialObject(SpecialObjects.Function))
        // todo write this to heap
        val resultHeap = initialHeap.end(mutator)

        state.copy(stack = obj :: state.stack, heap = resultHeap)
    }

    override def asAsmStmt: String = s"pushFunc $name"
}
