package inferium.dataflow

abstract class DataFlowAnalysis(val debugAdapter: DebugAdapter) {

    def enqueue(node: graph.Node)
}
