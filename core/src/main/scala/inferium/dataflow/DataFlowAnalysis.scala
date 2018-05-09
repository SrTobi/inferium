package inferium.dataflow

import scala.collection.mutable

class DataFlowAnalysis(analysable: Analysable, val debugAdapter: DebugAdapter = DebugAdapter.Empty) {

    private val enquedNodes = mutable.Set.empty[graph.Node]
    private val nodesToProcess = mutable.PriorityQueue.empty[graph.Node](Ordering.by(_.priority))

    def enqueue(node: graph.Node): Unit = {
        if (!enquedNodes(node)) {
            nodesToProcess.enqueue(node)
            enquedNodes += node
        }
    }

    def runAnalysis(initialState: ExecutionState): Unit = {
        analysable.begin.setNewInState(initialState)(this)
        runAnalysis()
    }

    def runAnalysis(): Unit = {
        enqueue(analysable.begin)

        while (nodesToProcess.nonEmpty) {
            val node = nodesToProcess.dequeue()
            enquedNodes -= node
            node.process(this)
        }

    }
}
