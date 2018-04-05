package inferium.dataflow

trait Analysable {
    def begin: graph.Node
    def end: graph.EndNode
}
