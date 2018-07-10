package inferium.dataflow

import inferium.dataflow.graph.Node.StateOrigin

import scala.collection.mutable

class ScriptAnalysis(analysable: Analysable, _debugAdapter: DebugAdapter = DebugAdapter.Empty) extends DataFlowAnalysis(_debugAdapter) {

    private implicit def useThisAsAnalysis: DataFlowAnalysis = this
    private val initialStateOrigin = StateOrigin()
    private val enquedNodes = mutable.Set.empty[graph.Node]
    private val nodesToProcess = mutable.PriorityQueue.empty[graph.Node](Ordering.by(_.priority))

    def enqueue(node: graph.Node): Unit = {
        if (!enquedNodes(node)) {
            nodesToProcess.enqueue(node)
            enquedNodes += node
        }
    }

    def runAnalysis(initialState: ExecutionState): Unit = {
        analysable.begin.setNewInState(initialState, initialStateOrigin)
        runAnalysis()
    }

    def runAnalysis(): Unit = {
        enqueue(analysable.begin)

        while (nodesToProcess.nonEmpty) {
            val node = nodesToProcess.dequeue()
            enquedNodes -= node
            node.process(this)
        }

        debugAdapter.finalizeAnalysis(analysable.begin)
    }
}
