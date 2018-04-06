package inferium.dataflow.graph.visitors

import inferium.dataflow.graph._
import inferium.utils.{Id, IdGenerator}

import scala.collection.mutable

class DotPrintVisitor(val showStackInfo: Boolean = false) extends Node.AllVisitor {

    private class Printer(buffer: mutable.StringBuilder = new mutable.StringBuilder(), indent: Int = 0, parent: Option[Printer] = None) {
        private var child: Option[Printer] = None
        private var done = false

        def line(line: String): this.type = {
            for (i <- 0 until indent)
                buffer append "  "
            buffer append line
            buffer append "\n"
            this
        }

        def indented(): Printer = {
            require(!done)
            require(child.isEmpty)
            val p = new Printer(buffer, indent + 1, Some(this))
            child = Some(p)
            p
        }

        def finish(): Unit = {
            require(!done)
            parent foreach { _.child = None }
            done = true
        }

        override def toString: String = {
            require(done)
            require(parent.isEmpty)
            return buffer.toString
        }
    }

    private val idGen = new IdGenerator[SubGraphMember]

    private abstract class SubGraphMember(catchTarget: =>Option[Node], top: Boolean = false) {
        val id: Id[SubGraphMember] = idGen.newId()

        val subGraph: SubGraphMember = (catchTarget map { target => nodeTarget.getOrElseUpdate(catchTarget, new SubGraph(target.catchTarget)) }) getOrElse (if (top) null else TopGraph)

        val members: mutable.Buffer[SubGraphMember] = mutable.Buffer.empty

        if (subGraph != null) {
            subGraph.members += this
        }

        def print(printer: Printer): Unit
    }

    private object TopGraph extends SubGraphMember(None, true) {
        override def print(printer: Printer): Unit = {
            for (member <- members) {
                member.print(printer)
            }
        }
    }

    private class SubGraph(catchTarget: =>Option[Node]) extends SubGraphMember(catchTarget) {
        override def print(printer: Printer): Unit = {
            printer.line(s"subgraph sub${id.id} {")
            val p = printer.indented()
            for (member <- members) {
                member.print(p)
            }
            p.finish()
            printer.line("}")
        }
    }

    private class BlockNode(catchTarget: Option[Node]) extends SubGraphMember(catchTarget) {
        val nodes: mutable.Buffer[Node] = mutable.Buffer.empty
        def nodeId: String = s"node${id.id}"

        override def print(printer: Printer): Unit = {
            assert(nodes.nonEmpty)
            nodes.last.successors map nodesToBlock foreach {
                succ =>
                    printer.line(s"$nodeId -> ${succ.nodeId} [label=${succ.nodes.head.label}]")
            }
        }

        def printLabel(printer: Printer): Unit = {
            val nodeText = nodes.map(_.asAsmStmt.replace("\"", "\\\"")).mkString("", "\\l", "\\l")
            printer.line(nodeId + "[label=\"" + nodeText +"\"]")
        }
    }

    private val nodesToBlock = mutable.Map.empty[Node, BlockNode]
    private val nodeTarget = mutable.Map.empty[Option[Node], SubGraphMember]


    override protected def visit(node: Node): Unit = {
        assert(!nodesToBlock.contains(node))
        val blk = node.predecessors match {
            case Seq(pred) if pred.successors.size == 1 && pred.catchTarget == node.catchTarget =>
                assert(nodesToBlock contains pred)
                nodesToBlock(pred)
            case _ =>
                new BlockNode(node.catchTarget)
        }

        blk.nodes += node
        nodesToBlock += node -> blk
    }

    override def toString: String = {
        val printer = new Printer()
        printer.line("digraph G {")
        val inner = printer.indented()
        inner.line("node [shape=box]")
        TopGraph.print(inner)
        nodesToBlock.values.toSeq.distinct foreach { _.printLabel(inner) }
        inner.finish()
        printer.line("}")
        printer.finish()
        printer.toString
    }
}
