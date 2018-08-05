package inferium.dataflow.graph.visitors

import inferium.dataflow.graph.{Graph, Node}

class ResetVisitor extends Node.AllVisitor{
    override protected def visit(node: Node): Unit = node.reset()
}

object ResetVisitor {
    def reset(graph: Graph): Unit = {
        (new ResetVisitor).start(graph)
    }
}