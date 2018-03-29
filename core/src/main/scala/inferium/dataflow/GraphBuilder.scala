package inferium.dataflow
import escalima.ast
import inferium.dataflow.graph.{LinearNode, MergeNode, Node}
import inferium.lattice.Entity

import scala.collection.mutable


object GraphBuilder {

    private class LabelManager {
        import LabelManager.Action
        private val labels = mutable.Map.empty[String, mutable.Buffer[Action]]
        private var loops: List[mutable.Buffer[Action]] = Nil

        def declareLabel(name: String): Unit = {
            val prev = labels.put(name, mutable.Buffer.empty)
            if (prev.isDefined) {
                throw new BuildException(s"Label '$name' is already declared")
            }
        }

        def defineLabel(name: String, g: Graph): Unit = {
            val usages = labels.remove(name).get
            usages foreach { _(g) }
        }

        def useLabel(name: String, action: Action): Unit = {
            labels.get(name) match {
                case None => throw new BuildException(s"Label '$name' was not declared")
                case Some(usages) =>
                    usages += action
            }
        }

        def declareLoop(): Unit = {
            loops ::= mutable.Buffer.empty
        }

        def defineLoop(g: Graph): Unit = {
            if (loops.isEmpty) {
                throw new BuildException("No loop defined")
            }
            val actions :: rest = loops
            loops = rest
            actions foreach { _(g) }
        }

        def useLoop(action: Action): Unit = {
            if (loops.isEmpty) {
                throw new BuildException("No loop defined")
            }
            loops.head += action
        }
    }

    private object LabelManager {
        type Action = (Graph) => Unit
    }

    private sealed abstract class Graph {

        def embedInto(before: LinearNode, after: Node): Unit
        def ++(graph: Graph): Graph
        def begin_<~(node: LinearNode): Unit
        def after_<~(node: LinearNode): Unit
    }

    private final case object EmptyGraph extends Graph {
        override def embedInto(before: LinearNode, after: Node): Unit = {
            before.next = after
        }

        override def ++(graph: Graph): Graph = graph

        override def begin_<~(node: LinearNode): Unit = throw new IllegalArgumentException("can not connect to empty graph")
        override def after_<~(node: LinearNode): Unit = throw new IllegalArgumentException("can not connect behind empty graph")

    }
    private final case class LinearGraph(begin: Node, end: LinearNode) extends Graph {
        protected var after: Node = _
        private var nodesToAfter: List[LinearNode] = Nil

        override def embedInto(before: LinearNode, after: Node): Unit = {
            before.next = begin
            end.next = after
            connectWaitingNodesToAfter(after)
        }

        override def ++(graph: Graph): Graph = {
            graph match {
                case EmptyGraph =>
                    this
                case LinearGraph(otherBegin, otherEnd) =>
                    end.next = otherBegin
                    connectWaitingNodesToAfter(otherBegin)
                    Graph(begin, otherEnd)
            }
        }

        override def begin_<~(node: LinearNode): Unit = {
            node.next = begin
        }


        override def after_<~(node: LinearNode): Unit = {
            if(after == null) {
                nodesToAfter ::= node
            } else {
                node.next = after
            }
        }

        private def connectWaitingNodesToAfter(after: Node): Unit = {
            nodesToAfter foreach { _.next = after }
            assert(this.after == null)
            this.after = after
        }
    }

    private object Graph {
        def apply(begin: Node, end: LinearNode): LinearGraph = LinearGraph(begin, end)
        def apply(beginAndEnd: LinearNode): LinearGraph = LinearGraph(beginAndEnd, beginAndEnd)
    }

    private implicit def convertNodeToGraph(node: LinearNode): LinearGraph = Graph(node)

    private class BlockInfo(outer: Option[BlockInfo],
                            labelTargets: Map[String, (MergeNode, MergeNode)],
                            loopTarget: Option[(MergeNode, MergeNode)],
                            catchTarget: Option[MergeNode],
                            finallizer: Option[() => Graph]) {

        val depth: Int = outer.map(_.depth + 1).getOrElse(0)
    }

    private class BlockBuilder(var strict: Boolean = false,
                               labels: LabelManager,
                               exceptionTarget: Option[Node]) {

        private var done = false
        private val hoistables = mutable.Buffer.empty[Node]

        private def labelConnector(label: Option[ast.Identifier]): (LabelManager.Action) => Unit = label match {
            case None => labels.useLoop
            case Some(ast.Identifier(name)) => labels.useLabel(name, _)
        }

        private def buildStatement(stmt: ast.Statement): Graph = stmt match {
            case ast.LabeledStatement(ast.Identifier(name), body) =>
                labels.declareLabel(name)
                val bodyGraph = buildStatement(body)
                labels.defineLabel(name, bodyGraph)
                return bodyGraph

            case ast.BlockStatement(body) =>
                val blockBuilder = new BlockBuilder(strict, labels, exceptionTarget)
                return blockBuilder.build(body)

            case ast.BreakStatement(label) =>
                val connectLabel = labelConnector(label)
                val jmp = new graph.JumpNode
                connectLabel((g) => g.after_<~(jmp))
                return jmp

            case ast.ContinueStatement(label) =>
                val connectLabel = labelConnector(label)
                val jmp = new graph.JumpNode
                connectLabel((g) => g.begin_<~(jmp))
                return jmp

            case ast.TryStatement(body, catchHandler, finallyHandler) =>


            // ignore
            case _: ast.DebuggerStatement =>
                return EmptyGraph
        }

        def build(statements: Seq[ast.Statement]): Graph = {
            assert(!done)

            // check that directives only appear at the beginning
            assert(statements.span(_.isInstanceOf[ast.Directive])._2.forall(!_.isInstanceOf[ast.Directive]))

            var graph: Graph = EmptyGraph
            var visitedNonDirective = false

            statements foreach {
                case ast.Directive(_, directive) =>
                    assert(!visitedNonDirective)
                    if (directive == "use strict;") {
                        strict = true
                    }

                case stmt =>
                    visitedNonDirective = true
                    graph ++= buildStatement(stmt)
            }
            graph
        }
    }

    def buildTemplate(scriptAst: ast.Program): Templates.Script =  new Templates.Script {
        override def instantiate(global: Entity): (Node, Node) = {
            (null, null)
        }
    }
}