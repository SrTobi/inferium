package inferium.dataflow.graph.visitors

import inferium.dataflow.graph._

class PrintVisitor(val showStackInfo: Boolean = false, val maxLines: Int = 1000) extends Node.AllVisitor {
    private val builder = new StringBuilder
    private var lineCount = 0

    private def cmd(text: String): Unit = {
        builder.append("  ")
        line(text)
    }

    private def line(text: String): Unit = {
        builder.append(text)
        builder.append("\n")
        lineCount += 1
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
        if (lineCount >= maxLines) {
            cmd("...")
            stop()
            return
        }

        val printLabel = needsLabel(node)
        val printPreStackInfo = (printLabel || lineCount == 0) && showStackInfo

        if (printLabel) {
            line(s"${node.label}:")
        }

        if (printPreStackInfo) {
            lazy val Seq(pred) =  node.predecessors.toSeq
            printStackInfo(if (lineCount == 0 || node.isInstanceOf[MergeNode]) node else pred)
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
