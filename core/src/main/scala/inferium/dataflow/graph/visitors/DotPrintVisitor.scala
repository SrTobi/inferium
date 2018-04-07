package inferium.dataflow.graph.visitors

import inferium.dataflow.graph._
import inferium.utils.{Id, IdGenerator}

import scala.collection.{Traversable, mutable}

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

    private abstract class SubGraphMember(catchTarget: Option[Node], isTop: Boolean = false) {
        val id: Id[SubGraphMember] = idGen.newId()

        val subGraph: Option[SubGraphMember] = if (isTop) {
            None
        } else {
            catchTarget map {
                target =>
                    nodeTarget.getOrElseUpdate(target, new SubGraph(target))
            } orElse Some(TopGraph)
        }

        val members: mutable.Buffer[SubGraphMember] = mutable.Buffer.empty

        subGraph foreach { _.members += this }

        def print(printer: Printer): Unit
    }

    private class SubGraph(targetNode: Node, isTop: Boolean = false) extends SubGraphMember(targetNode.catchTarget, isTop) {
        override def print(printer: Printer): Unit = {
            val subGraphId = s"cluster_${id.id}"
            printer.line(s"subgraph $subGraphId {")
            val p = printer.indented()
            //p.line("color=lightgrey;")
            for (member <- members) {
                member.print(p)
            }
            p.finish()
            printer.line("}")

            // exception edge
            nodesToBlock get targetNode foreach {
                block =>
                    printer.line(s"${members.collect{ case blk: BlockNode => blk}.last.blockId} -> ${block.blockId} [ltail=$subGraphId,color=red]")
            }
        }
    }

    private object TopGraph extends SubGraphMember(None, true) {
        override def print(printer: Printer): Unit = {
            for (member <- members) {
                member.print(printer)
            }
        }
    }

    private class BlockNode(catchTarget: Option[Node]) extends SubGraphMember(catchTarget) {
        val nodes: mutable.Buffer[Node] = mutable.Buffer.empty
        def blockId: String = s"node${id.id}"

        override def print(printer: Printer): Unit = {
            assert(nodes.nonEmpty)
            val succs = nodes.last match {
                case node: JumpNode => Seq(node.target)
                case node => node.successors.filter(!node.catchTarget.contains(_))
            }

            succs map nodesToBlock foreach {
                succ =>
                    printer.line(s"$blockId -> ${succ.blockId} [label=${succ.nodes.head.label}]")
            }
        }

        private def nodeToText(node: Node, builder: mutable.StringBuilder): Unit = {
            def appendStack(node: Node): Unit = {
                if (showStackInfo ) {
                    builder.append(s"# Stack: ${node.exprStackInfo.mkString("[", " :: ", "]")}\n")
                }
            }

            if (!node.isInstanceOf[MergeNode]) {
                node.predecessors match {
                    case Seq(pred) if nodesToBlock(pred) != this =>
                        appendStack(pred)
                    case _ =>
                }
            }

            builder.append(node.asAsmStmt + "\n")

            if (node.successors.length == 1) {
                appendStack(node)
            }
        }

        def printLabel(printer: Printer): Unit = {
            val builder = new mutable.StringBuilder
            nodes foreach { n => nodeToText(n, builder) }
            val nodeText = builder.toString.replace("""\""", """\\""").replace("\"", "\\\"").replace("\n", "\\l")
            printer.line(blockId + " [label=\"" + nodeText +"\"]")
        }
    }

    private val nodesToBlock = mutable.Map.empty[Node, BlockNode]
    private val nodeTarget = mutable.Map.empty[Node, SubGraph]


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
        inner.line("compound=true")
        inner.line("node [shape=box]")
        TopGraph.print(inner)
        nodesToBlock.values.toSeq.distinct foreach { _.printLabel(inner) }
        inner.finish()
        printer.line("}")
        printer.finish()
        printer.toString
    }
}
