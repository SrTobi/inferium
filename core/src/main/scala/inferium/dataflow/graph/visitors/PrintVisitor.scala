package inferium.dataflow.graph.visitors

import inferium.dataflow.calls.CallInstance
import inferium.dataflow.graph._
import inferium.lattice.Location

import scala.collection.mutable

private class PrintVisitor(val showStackInfo: Boolean,
                           val showNodeInfo: Boolean,
                           val printMergeNodes: Boolean,
                           val printEmptyLineAfterNode: Boolean,
                           val maxLines: Int) extends Node.AllVisitor {
    private val builder = new StringBuilder
    private var lineCount = 0
    private var firstNode: Boolean = true
    private var isSubcall: Boolean = false
    private val subCallQueue = mutable.Queue.empty[Node]
    private val foundSubcalls = mutable.Set.empty[Node]

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
        node.predecessors.size > 1 || (node.predecessors match {
            case Seq(pred) if pred.priority > node.priority => true
            case Seq(_: JumpNode)  => true
            case Seq(_: CondJumpNode) => true
            case _ => false
        })
    }

    private def printGoto(node: Node): Unit = {
        node.successors match {
            case Seq(target: MergeNode) =>
                if (target.mergeType == MergeNode.MergeType.CallMerger) {
                    cmd("ret")
                } else if (target.mergeType == MergeNode.MergeType.CatchMerger || target.predecessors.length > 1) {
                    cmd(s"goto ${target.label}")
                }
            case _ =>
        }
    }

    private def printStackInfo(node: Node): Unit = {
        if (showStackInfo) {
            assert(node.exprStackInfo != null)
            cmd(s"# Stack:       ${node.exprStackInfo.mkString("[", " :: ", "]")}")
        }
    }

    private def printNodeInfo(node: Node): Unit = {
        if (showNodeInfo) {
            val info = node.info
            cmd(s"# Id:          ${node.id}")
            cmd(s"# Priority:    ${info.priority}")
            cmd(s"# CatchTarget: ${info.catchTarget.getOrElse("none")}")
            cmd(s"# LexicalEnv:  ${info.lexicalEnv.toString(true)}")
            info.label match {
                case Some(label) =>
                    cmd(s"# Label:       $label")
                case None =>
            }
            for (Node.AdditionalInfo(key, value) <- node.additionalInfos) {
                cmd(s"# $key: $value")
            }
        }
    }

    override protected def visit(node: Node): Unit = {
        if (lineCount >= maxLines) {
            cmd("...")
            stop()
            return
        }

        val printLabel = needsLabel(node)
        val printPreStackInfo = (printLabel || firstNode) && node.predecessors.length <= 1 && showStackInfo
        firstNode = false

        if (printLabel) {
            line(s"${node.label}:")
        }

        if (printPreStackInfo) {
            if (node.predecessors.isEmpty) {
                if (isSubcall) {
                    cmd("# Stack:       [arguments]")
                } else {
                    cmd("# Stack:       [unknown]")
                }
            } else {
                lazy val Seq(pred) =  node.predecessors
                printStackInfo(pred)
            }
        }

        val printStmt = node match {
            case _: MergeNode =>
                printMergeNodes
            case _: EndNode =>
                false
            case _ => true
        }

        if (printStmt) {
            printNodeInfo(node)
            cmd(node.asAsmStmt)

            node match {
                case node: CallNode =>
                    import CallInstance._

                    node.calls foreach {
                        case InlinedCallInfo(begin) =>
                            if (!foundSubcalls(begin)) {
                                foundSubcalls += begin
                                subCallQueue += begin
                            }
                            cmd(s"  -inlined: ${begin.id}")

                        case NativeCallInfo(_) =>
                            ???

                        case SignatureCallInfo() =>
                            ???

                        case RecursiveCallInfo() =>
                            ???
                    }

                case _ =>
            }
        }

        val printStack = node match {
            case _: JumpNode => false
            case _: CondJumpNode => false
            case _: MergeNode => false
            case _: EndNode => false
            case _ => true
        }

        if (printStack) {
            printStackInfo(node)
        }
        printGoto(node)

        if (node.successors.isEmpty) {
            cmd("end")
        } else if (printEmptyLineAfterNode) {
            cmd("")
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

    def printSubCalls(): Unit = {
        isSubcall = true
        while (subCallQueue.nonEmpty) {
            val subcallBegin = subCallQueue.dequeue()
            line("")
            line(s"Subcall to ${subcallBegin.id}")
            firstNode = true
            start(subcallBegin)
        }
    }
}

object PrintVisitor {
    def print(graph: Graph,
              showStackInfo: Boolean = false,
              showNodeInfo: Boolean = false,
              printMergeNodes: Boolean = false,
              printSubCalls: Boolean = true,
              maxLines: Int = 1000): String = {
        val visitor = new PrintVisitor(
            showStackInfo,
            showNodeInfo,
            printMergeNodes,
            printEmptyLineAfterNode = showNodeInfo,
            maxLines
        ).start(graph)

        if (printSubCalls) {
            visitor.printSubCalls()
        }

        visitor.toString
    }
}