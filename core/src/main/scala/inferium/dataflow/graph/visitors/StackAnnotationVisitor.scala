package inferium.dataflow.graph.visitors

import inferium.dataflow.graph
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.Node.ExprStackFrame

class StackAnnotationVisitor extends Node.AllVisitor {
    override protected def visit(node: Node): Unit = {
        val predecesors = node.predecessors.toSeq
        val preds = predecesors.filter(Node.isForwardEdge(_, node))
        lazy val Seq(pred) = preds
        lazy val startStack: List[ExprStackFrame] = "unknown" :: Nil
        lazy val stack = if (preds.isEmpty) startStack else pred.exprStackInfo

        implicit def everythingToStackFrame(any: Any): ExprStackFrame = ExprStackFrame(any.toString)

        assert(node.exprStackInfo == null)

        assert(preds forall {_.exprStackInfo != null})

        node.exprStackInfo = node match {
            case node: graph.LiteralNode =>
                node.literal :: stack

            case _: graph.PopNode =>
                assert(stack.nonEmpty, "expression stack should contain an item which can be popped")
                stack.tail

            case _: graph.JumpNode =>
                stack

            case _: graph.CondJumpNode =>
                stack.tail // the jump reads the condition

            case merger: graph.MergeNode =>
                assert(preds forall { _.exprStackInfo.nonEmpty })

                val stack = preds.head.exprStackInfo.tail
                assert(predecesors forall { _.exprStackInfo.tail eq stack })
                val mergeFrame = predecesors map { p => if (p.exprStackInfo == null) ExprStackFrame(p.toString) else p.exprStackInfo.head } reduce[ExprStackFrame] {
                    case (left, right) =>
                        ExprStackFrame("|", left, right)
                }
                mergeFrame :: Nil

            case _: graph.PushLexicalFrame =>
                stack

            case _: graph.LexicalWriteNode =>
                stack.tail

            case node: graph.LexicalReadNode =>
                node.varName :: stack
        }
    }
}
