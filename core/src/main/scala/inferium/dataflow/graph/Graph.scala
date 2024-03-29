package inferium.dataflow.graph

import inferium.dataflow.Analysable
import inferium.dataflow.graph.visitors.PrintVisitor
import scala.language.implicitConversions


sealed abstract class Graph {

    def ~>(graph: Graph): Graph
    def ~>(node: Node): Graph = this ~> Graph(node)

    def begin(after: => Node): Node
    def begin: Node
    def beginOption: Option[Node]

    def end(before: => Node): Node
    def end: Node
    def endOption: Option[Node]

    def replace(oldNode: Node, newNode: Node): Graph
}

case object EmptyGraph extends Graph {
    override def ~>(graph: Graph): Graph = graph

    override def replace(oldNode: Node, newNode: Node): Graph = EmptyGraph

    override def begin(after: => Node): Node = after
    override def begin: Node = throw new IllegalAccessException("can not access first node of an empty graph")
    override def beginOption: Option[Node] = None

    override def end(before: => Node): Node = before
    override def end: Node = throw new IllegalAccessException("can not access last node of an empty graph")
    override def endOption: Option[Node] = None

    override def toString: String = "EmptyGraph"
}

sealed abstract class NonEmptyGraph extends Graph {
    override def begin: Node
    override def end: Node
    def priority: Int


    override def begin(after: => Node): Node = begin
    override def beginOption: Option[Node] = Some(begin)

    override def end(before: => Node): Node = end
    override def endOption: Option[Node] = Some(end)

    override def toString: String = PrintVisitor.print(this)
}

final case class GraphPath(override val begin: Node, override val end: Node)(override val priority: Int = Math.min(begin.priority, end.priority)) extends NonEmptyGraph {
    override def ~>(graph: Graph): Graph = {
        graph match {
            case EmptyGraph =>
                this
            case g@Graph(otherBegin, otherEnd, otherPriority) =>
                end ~> otherBegin
                GraphPath(begin, otherEnd)(Math.min(priority, otherPriority))
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
}

final case class ScriptGraph(override val begin: Node, override val end: EndNode) extends NonEmptyGraph with Analysable {
    override def priority: Int = 0

    override def ~>(graph: Graph): Graph = throw new IllegalAccessException("Can not connect script graph to another graph")

    override def replace(oldNode: Node, newNode: Node): Graph = ???


    override def begin(after: => Node): Node = end
}

object Graph {
    implicit def convertSingleNodeToGraph(node: Node): Graph = Graph(node)

    def concat(graphs: TraversableOnce[Graph]): Graph = graphs.foldLeft[Graph](EmptyGraph) { _ ~> _ }

    def apply(): EmptyGraph.type = EmptyGraph
    def apply(beginAndEnd: Node): GraphPath = GraphPath(beginAndEnd, beginAndEnd)()
    def apply(begin: Node, end: Node): GraphPath = GraphPath(begin, end)()
    def apply(beginAndEnd: Option[Node]): Graph = beginAndEnd.map(Graph(_)).getOrElse(EmptyGraph)

    def unapply(graph: Graph): Option[(Node, Node, Int)] = graph match {
        case graph: NonEmptyGraph => Some((graph.begin, graph.end, graph.priority))
        case EmptyGraph => None
    }
}