package inferium.dataflow.graph

import inferium.dataflow.{CallableInfo, DataFlowAnalysis, ExecutionState, Templates}
import inferium.dataflow.graph.traits.TransformerNode
import inferium.lattice.FunctionEntity
import inferium.lattice.Heap.SpecialObjects

class AllocateFunctionNode(val function: CallableInfo, val captureThis: Boolean)(implicit _info: Node.Info) extends TransformerNode {
    def name: String = function.name.getOrElse("<anonym>")

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): ExecutionState = {
        val initialHeap = state.heap

        val mutator = initialHeap.begin(loc)
        val obj = mutator.allocObject(loc, (l, ac) => {
            FunctionEntity(l, state.lexicalFrame)(ac, function)
        }, initialHeap.specialObject(SpecialObjects.Function))
        // todo write this to heap
        val resultHeap = initialHeap.end(mutator)

        state.copy(stack = obj :: state.stack, heap = resultHeap)
    }

    override def asAsmStmt: String = s"pushFunc $name"
}
