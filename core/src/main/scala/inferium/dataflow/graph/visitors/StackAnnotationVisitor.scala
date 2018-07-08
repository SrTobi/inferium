package inferium.dataflow.graph.visitors

import inferium.dataflow.graph
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.Node.ExprStackFrame
import inferium.lattice.UndefinedValue
import scala.language.implicitConversions

class StackAnnotationVisitor(isFunction: Boolean) extends Node.AllVisitor {
    override protected def visit(node: Node): Unit = {
        implicit def everythingToStackFrame(any: Any): ExprStackFrame = ExprStackFrame(any.toString)

        val predecesors = node.predecessors
        val preds = predecesors.filter(Node.isForwardEdge(_, node))
        val undefined: ExprStackFrame = UndefinedValue
        lazy val Seq(pred) = preds
        lazy val startStack: List[ExprStackFrame] = if (isFunction) "arguments" :: Nil else "unknown" :: Nil
        lazy val stack = if (preds.isEmpty) startStack else pred.exprStackInfo


        assert(node.exprStackInfo == null)

        assert(preds forall {_.exprStackInfo != null})
        assert(preds.isEmpty || (preds zip preds.tail forall { case (fst, snd) => fst.exprStackInfo.length == snd.exprStackInfo.length } ))

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
                assert(isFunction || (preds forall { _.exprStackInfo.nonEmpty }))

                if(merger.isCatchMerger) {
                    assert(preds.count(!_.catchTarget.contains(merger)) == 1)
                    "exception" :: undefined :: Nil
                } else {
                    val stackIsEmpty = preds.head.exprStackInfo.isEmpty
                    if (stackIsEmpty) {
                        assert(isFunction)
                        Nil
                    } else {
                        val restStack = preds.head.exprStackInfo.tail
                        assert(preds forall {
                            _.exprStackInfo.tail eq restStack
                        })
                        val mergeFrame = predecesors map { p => if (p.exprStackInfo == null) ExprStackFrame(p.toString) else p.exprStackInfo.head } reduce[ExprStackFrame] {
                            case (left, right) =>
                                ExprStackFrame("|", left, right)
                        }

                        mergeFrame :: restStack
                    }
                }

            case _: graph.DebugNode =>
                stack

            case node: graph.DebugSquashNode =>
                val (args, rest) = stack.splitAt(node.squashNum)
                ExprStackFrame("|", args) :: rest

            case node: graph.PushLexicalFrameNode =>
                if (node.takeFromStack) stack.tail else stack

            case _: graph.LexicalWriteNode =>
                stack

            case node: graph.LexicalReadNode =>
                node.varName :: stack

            case _: graph.EndNode =>
                assert(stack.length == 1)
                stack

            case _: graph.RetNode =>
                assert(stack.length == 1)
                stack

            case _: graph.PushThisNode =>
                "this" :: stack

            case node: graph.AllocateObjectNode =>
                s"obj#${node.id}" :: stack

            case node: graph.AllocateArrayNode =>
                s"arr#${node.id}" :: stack.drop(node.elementsOnStackCount)

            case _: graph.PropertyWriteNode =>
                val writeValue :: /* base object */ _ :: rest = stack
                writeValue :: rest

            case _: graph.PropertyDynamicWriteNode =>
                val writeValue :: _ /* property */ :: _ /* base object */ :: rest = stack
                writeValue :: rest

            case node: graph.PropertyReadNode =>
                val obj :: rest = stack
                ExprStackFrame("read", obj, node.propertyName) :: rest

            case node: graph.PropertyDynamicReadNode =>
                val property :: obj :: rest = stack
                ExprStackFrame("read", obj, property) :: rest

            case node: graph.DupNode =>
                val top :: _ = stack
                List.fill(node.times)(top) ++ stack

            case node: graph.AllocateFunctionNode =>
                s"func(${node.name})" :: stack

            case node: graph.CallNode =>
                // drop arguments
                // drop function
                // drop this
                val stackWithoutArgs = stack.drop(node.argumentCount)
                val func :: stackWithThis = stackWithoutArgs
                val restStack = if (node.thisIsOnStack) stackWithThis.tail else stackWithThis
                ExprStackFrame("ret:", func) :: restStack

            case node: graph.NewNode =>
                // drop arguments
                // drop function
                val stackWithoutArgs = stack.drop(node.argumentCount)
                val func :: restStack = stackWithoutArgs
                ExprStackFrame("new:", func) :: restStack
        }
    }
}
