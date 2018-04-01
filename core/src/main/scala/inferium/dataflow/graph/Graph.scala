package inferium.dataflow.graph

import inferium.dataflow.graph.visitors.PrintVisitor


sealed abstract class Graph {

    def ~>(graph: Graph): Graph
    def ~>(node: Node): Graph = this ~> Graph(node)

    def begin(after: => Node): Node
    def begin: Node

    def replace(oldNode: Node, newNode: Node): Graph
}

case object EmptyGraph extends Graph {
    override def ~>(graph: Graph): Graph = graph

    override def replace(oldNode: Node, newNode: Node): Graph = EmptyGraph

    override def begin(after: => Node): Node = after
    override def begin: Node = throw new IllegalAccessException("can not access first node of an empty graph")

    override def toString: String = "EmptyGraph"
}
final case class GraphPath(override val begin: Node, end: Node)(val priority: Int = Math.min(begin.priority, end.priority)) extends Graph {
    override def ~>(graph: Graph): Graph = {
        graph match {
            case EmptyGraph =>
                this
            case g@GraphPath(otherBegin, otherEnd) =>
                end ~> otherBegin
                GraphPath(begin, otherEnd)(Math.min(this.priority, g.priority))
        }
    }

    override def replace(oldNode: Node, newNode: Node): Graph = {
        ???
        /*oldNode.replace(newNode)

        if (oldNode == begin || oldNode == end) {
            val newBegin = if (oldNode == begin) newNode else begin
            val newEnd = if (oldNode == end) newNode else end
            Graph(newBegin, newEnd)
        } else {
            this
        }*/
    }

    override def begin(after: => Node): Node = begin

    override def toString: String = new PrintVisitor().start(this).toString
}

object Graph {
    implicit def convertSingleNodeToGraph(node: Node): Graph = Graph(node)
    def apply(): EmptyGraph.type = EmptyGraph
    def apply(beginAndEnd: Node): GraphPath = GraphPath(beginAndEnd, beginAndEnd)()
    def apply(begin: Node, end: Node): GraphPath = GraphPath(begin, end)()
}