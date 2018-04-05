package inferium.dataflow.graph.visitors

import inferium.dataflow.graph._

class DotPrintVisitor(val showStackInfo: Boolean = false) extends Node.AllVisitor {
    private val builder = new StringBuilder
    private var first = true

    private def cmd(text: String): Unit = {
        builder.append("  ")
        line(text)
    }

    private def line(text: String): Unit = {
        builder.append(text)
        builder.append("\n")
    }

    private def needsLabel(node: Node): Boolean = {
        node.predecessors.size > 1 || (node.predecessors.toSeq match {
            case Seq(pred) if pred.priority > node.priority => true
            case Seq(_: JumpNode)  => true
            case Seq(_: CondJumpNode) => true
            case _ => false
        })
    }

    private def printGoto(node: Node): Unit = {
        node.successors match {
            case Seq(target: MergeNode) => cmd(s"goto ${target.label}")
            case _ =>
        }
    }

    private def printStackInfo(node: Node): Unit = {
        if (showStackInfo) {
            assert(node.exprStackInfo != null)
            cmd(s"# Stack: ${node.exprStackInfo.mkString("[", " :: ", "]")}")
        }
    }

    override protected def visit(node: Node): Unit = {
        val printLabel = needsLabel(node)
        val printPreStackInfo = (first || printLabel) && showStackInfo

        if (printLabel) {
            line(s"${node.label}:")
        }

        if (printPreStackInfo) {
            lazy val Seq(pred) =  node.predecessors.toSeq
            printStackInfo(if (node.isInstanceOf[MergeNode]) node else pred)
        }

        val printStack = node match {
            case jmp: JumpNode =>
                cmd(s"jmp ${jmp.target.label}")
                false

            case jmp: CondJumpNode =>
                cmd(s"cond ${jmp.thenNode.label}, ${jmp.elseNode.label}")
                false

            case node: LiteralNode =>
                cmd(s"push ${node.literal}")

            case _: MergeNode =>
                false

            case _: PopNode =>
                cmd("pop")

            case _: PushLexicalFrame =>
                cmd("lexPush")

            case node: LexicalReadNode =>
                cmd(s"read ${node.varName}")

            case node: LexicalWriteNode =>
                cmd(s"write ${node.varName}")

            case _: EndNode =>
                false
        }

        if (!printStack.equals(false)) {
            printStackInfo(node)
        }
        printGoto(node)

        if (node.successors.isEmpty) {
            cmd("end")
        }

        first = false
    }

    override def toString: String = {
        val result = builder.toString()
        if (result == "") {
            "empty"
        } else {
            result
        }
    }
}
