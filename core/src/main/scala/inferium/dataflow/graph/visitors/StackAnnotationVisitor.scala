package inferium.dataflow.graph.visitors

import inferium.dataflow.graph
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.Node.ExprStackFrame
import inferium.lattice.UndefinedValue

class StackAnnotationVisitor extends Node.AllVisitor {
    override protected def visit(node: Node): Unit = {
        implicit def everythingToStackFrame(any: Any): ExprStackFrame = ExprStackFrame(any.toString)

        val predecesors = node.predecessors
        val preds = predecesors.filter(Node.isForwardEdge(_, node))
        val undefined: ExprStackFrame = UndefinedValue
        lazy val Seq(pred) = preds
        lazy val startStack: List[ExprStackFrame] = "unknown" :: Nil
        lazy val stack = if (preds.isEmpty) startStack else pred.exprStackInfo


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

                if(merger.isCatchMerger) {
                    assert(preds.count(!_.catchTarget.contains(merger)) == 1)
                    "exception" :: undefined :: Nil
                } else {
                    val restStack = preds.head.exprStackInfo.tail
                    assert(predecesors forall {
                        _.exprStackInfo.tail eq restStack
                    })
                    val mergeFrame = predecesors map { p => if (p.exprStackInfo == null) ExprStackFrame(p.toString) else p.exprStackInfo.head } reduce[ExprStackFrame] {
                        case (left, right) =>
                            ExprStackFrame("|", left, right)
                    }

                    mergeFrame :: restStack
                }

            case _: graph.DebugNode =>
                stack

            case _: graph.PushLexicalFrame =>
                stack

            case _: graph.LexicalWriteNode =>
                stack.tail

            case node: graph.LexicalReadNode =>
                node.varName :: stack

            case node: graph.EndNode =>
                assert(stack.tail.isEmpty)
                stack
        }
    }
}
