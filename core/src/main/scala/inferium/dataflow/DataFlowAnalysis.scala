package inferium.dataflow

import scala.collection.mutable

class DataFlowAnalysis(subject: Analysable) {

    private val contextsToPropagate = mutable.Queue.empty[(ExecutionState, graph.Node)]

    def noStateFlowsTo(node: graph.Node): Unit = {

    }

    def stateFlowsTo(ctx: ExecutionState, node: graph.Node): Unit = {
        assert(ctx ne null)
        assert(node ne null)
        contextsToPropagate.enqueue(ctx -> node)
    }

    def runAnalysis(): Unit = {
        while (contextsToPropagate.nonEmpty) {
            val (ctx, node) = contextsToPropagate.dequeue()
            node.onControlFlow(ctx)
        }
    }
}
