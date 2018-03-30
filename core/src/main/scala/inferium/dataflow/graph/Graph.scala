package inferium.dataflow.graph


sealed abstract class Graph {

    def ~>(graph: Graph): Graph
    def ~>(node: Node): Graph = this ~> Graph(node)
    
    def replace(oldNode: Node, newNode: Node): Graph
}

case object EmptyGraph extends Graph {
    override def ~>(graph: Graph): Graph = graph

    override def replace(oldNode: Node, newNode: Node): Graph = EmptyGraph
}
final case class GraphPath(begin: Node, end: Node) extends Graph {
    override def ~>(graph: Graph): Graph = {
        graph match {
            case EmptyGraph =>
                this
            case GraphPath(otherBegin, otherEnd) =>
                end ~> otherBegin
                Graph(begin, otherEnd)
        }
    }

    override def replace(oldNode: Node, newNode: Node): Graph = {
        oldNode.replace(newNode)

        if (oldNode == begin || oldNode == end) {
            val newBegin = if (oldNode == begin) newNode else begin
            val newEnd = if (oldNode == end) newNode else end
            Graph(newBegin, newEnd)
        } else {
            this
        }
    }
}

object Graph {
    implicit def convertSingleNodeToGraph(node: Node): Graph = Graph(node)
    def apply(): EmptyGraph.type = EmptyGraph
    def apply(beginAndEnd: Node): GraphPath = GraphPath(beginAndEnd, beginAndEnd)
    def apply(begin: Node, end: Node): GraphPath = GraphPath(begin, end)
}