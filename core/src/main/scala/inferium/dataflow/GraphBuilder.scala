package inferium.dataflow
import escalima.ast
import inferium.dataflow.graph._
import inferium.lattice._

import scala.collection.mutable
import scala.util.Try


object GraphBuilder {

    private case class JumpTarget(targetBlock: BlockInfo, entry: Option[MergeNode], exit: MergeNode)

    private class BlockInfo(val outer: Option[BlockInfo],
                            val labelTargets: Map[String, JumpTarget],
                            val loopTarget: Option[JumpTarget],
                            val catchEntry: Option[MergeNode],
                            val finalizer: Option[() => Graph]) {

        val depth: Int = outer.map(_.depth + 1).getOrElse(0)

        def inner(labelTargets: Map[String, JumpTarget] = this.labelTargets,
                  loopTarget: Option[JumpTarget] = this.loopTarget,
                  catchEntry: Option[MergeNode] = None,
                  finalizer: Option[() => Graph] = None): BlockInfo = new BlockInfo(Some(this), labelTargets, loopTarget, catchEntry, finalizer)
    }

    private class BlockBuilder(var strict: Boolean = false,
                               block: BlockInfo) {

        private var done = false
        private val hoistables = mutable.Buffer.empty[Node]

        private def buildJump(targetBlock: BlockInfo, targetNode: MergeNode): graph.JumpNode = {
            def unroll(block: BlockInfo): Graph = {
                if (block == targetBlock) {
                    EmptyGraph
                }
                val finalizerGraph = block.finalizer.map(_()).getOrElse(EmptyGraph)
                val restGraph = block.outer.map(unroll).getOrElse(EmptyGraph)
                finalizerGraph ~> restGraph
            }

            val finalizers = unroll(block)
            val jmp = new graph.JumpNode
            jmp ~> finalizers ~> targetNode
            jmp
        }

        private def resolveLoopTarget(label: Option[ast.Identifier], labels: Map[String, JumpTarget]): JumpTarget = {
            label match {
                case Some(ast.Identifier(name)) =>
                    labels.get(name) match {
                        case Some(target) => target
                        case None => throw new BuildException(s"Label '$name' is not declared")
                    }

                case None =>
                    block.loopTarget match {
                        case Some(target) => target
                        case None => throw new BuildException(s"There is no loop to jump to")
                    }
            }
        }

        private def buildLiteral(entity: Entity): Graph = {
            new graph.LiteralNode(entity)
        }

        private def buildExpression(expr: ast.Expression): Graph = expr match {
            case ast.BooleanLiteral(value) =>
                buildLiteral(BoolValue(BoolLattice(value)))

            case ast.StringLiteral(value) =>
                buildLiteral(StringValue(value))

            case num: ast.NumberLiteral =>
                val value = Try(num.raw.toInt).map(SpecificNumberValue).getOrElse(NumberValue)
                buildLiteral(value)

            case _: ast.NullLiteral =>
                buildLiteral(NullValue)

            case ast.Identifier("undefined") =>
                buildLiteral(UndefinedValue)

            case ast.Identifier(varName) =>
                new graph.LexicalContextReadNode(varName)
        }

        private def buildStatement(stmt: ast.Statement, labels: Map[String, JumpTarget], here: Option[JumpTarget]): Graph = stmt match {
            case ast.LabeledStatement(ast.Identifier(name), body) =>
                if (labels.contains(name)) {
                    throw new BuildException(s"Label '$name' already declared")
                }

                val (target, merger) = here match {
                    case Some(hereTarget) =>
                        (hereTarget, EmptyGraph)
                    case None =>
                        val merger = new MergeNode(true)
                        (JumpTarget(block, None, merger), Graph(merger))
                }
                val graph = buildStatement(body, labels + (name -> target), Some(target))
                graph ~> merger

            case ast.BlockStatement(body) =>
                val innerBlock = block.inner(labelTargets = labels)
                buildInnerBlock(innerBlock, body)

            case ast.BreakStatement(targetLabel) =>
                val target = resolveLoopTarget(targetLabel, labels)
                buildJump(target.targetBlock, target.exit)

            case ast.ContinueStatement(targetLabel) =>
                val target = resolveLoopTarget(targetLabel, labels)
                val loopEntry = target.entry.getOrElse(throw new BuildException(s"Can not continue the statement '${targetLabel.map(_.name).getOrElse("???")}'"))
                buildJump(target.targetBlock, loopEntry)

            case ast.ExpressionStatement(expr) =>
                val exprGraph = buildExpression(expr)
                exprGraph ~> new graph.PopNode

            case ast.TryStatement(tryBody, catchHandler, finallyHandler) =>
                val catchMerger = new graph.MergeNode

                val finallyBuilder = finallyHandler match {
                    case Some(ast.BlockStatement(finallyBody)) =>
                        () => buildInnerBlock(block.inner(), finallyBody)
                    case None =>
                        () => EmptyGraph
                }

                val tryBlock = block.inner(catchEntry = Some(catchMerger), finalizer = Some(finallyBuilder))
                val tryGraph = buildInnerBlock(tryBlock, tryBody.body)

                // build catch
                val afterCatch = catchHandler match {
                    case Some(ast.CatchClause(pattern, catchBody)) =>
                        ???
                    case None =>
                        catchMerger
                }

                val tryCatchGraph = tryGraph ~> afterCatch

                // build finally
                val finallyGraph = finallyBuilder()

                return tryCatchGraph ~> finallyGraph

            case _: ast.DebuggerStatement =>
                return EmptyGraph
            case _: ast.Directive =>
                throw new AssertionError("Directives should not appear here")
        }


        private def buildInnerBlock(block: BlockInfo, statements: Seq[ast.Statement]): Graph = {
            val builder = new BlockBuilder(strict, block)
            builder.build(statements)
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
                    val stmtGraph = buildStatement(stmt, block.labelTargets, None)
                    graph = graph ~> stmtGraph
            }
            done = true
            graph
        }
    }

    def buildTemplate(scriptAst: ast.Program): Templates.Script =  new Templates.Script {
        override def instantiate(global: Entity): (Node, Node) = {
            (null, null)
        }
    }
}