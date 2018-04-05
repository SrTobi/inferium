package inferium.dataflow

import scala.collection.mutable

class DataFlowAnalysis(analysable: Analysable, val debugAdapter: DebugAdapter = DebugAdapter.Empty) {

    private val nodesToProcess = mutable.PriorityQueue.empty[(graph.Node)](Ordering.by(_.priority))

    def enqueue(node: graph.Node): Unit = {
        nodesToProcess.enqueue(node)
    }

    def runAnalysis(initialState: ExecutionState): Unit = {
        analysable.begin.setNewInState(initialState)(this)
        runAnalysis()
    }

    def runAnalysis(): Unit = {
        nodesToProcess += analysable.begin

        while (nodesToProcess.nonEmpty) {
            val node = nodesToProcess.dequeue()
            node.process(this)
        }

    }
}
