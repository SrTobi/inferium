package inferium.dataflow
import escalima.ast
import inferium.Config.ConfigKey
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

    case class Config(bindLetAndConstToGlobal: Boolean)
    object Config {
        val bindLetAndConstToGlobal: ConfigKey[Boolean] = ConfigKey(false)


        implicit def configToGraphBuilder(config: inferium.Config): GraphBuilder.Config = GraphBuilder.Config(
            bindLetAndConstToGlobal = config(bindLetAndConstToGlobal)
        )
    }
}

class GraphBuilder(config: GraphBuilder.Config) {
    import GraphBuilder._

    private class BlockBuilder(var strict: Boolean = false,
                               block: BlockInfo) {

        private var done = false
        private val hoistables = mutable.Buffer.empty[Node]

        private def buildJump(targetBlock: BlockInfo, targetNode: MergeNode, priority: Int): graph.JumpNode = {
            def unroll(block: BlockInfo): Graph = {
                if (block == targetBlock) {
                    EmptyGraph
                }
                val finalizerGraph = block.finalizer.map(_()).getOrElse(EmptyGraph)
                val restGraph = block.outer.map(unroll).getOrElse(EmptyGraph)
                finalizerGraph ~> restGraph
            }

            val finalizers = unroll(block)
            val unrollGraph = finalizers ~> targetNode
            val jmp = new graph.JumpNode(unrollGraph.begin)(Node.Info(priority, targetBlock.catchEntry))
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

        private def buildLiteral(entity: Entity)(implicit info: Node.Info): Graph = {
            new graph.LiteralNode(entity)
        }

        private def buildExpression(expr: ast.Expression, priority: Int): Graph = {
            implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry)
            expr match {
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
        }

        private def buildExressionStatement(expr: ast.Expression, priority: Int): Graph = {
            implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry)

            val exprGraph = buildExpression(expr, priority)
            new PopNode() ~> exprGraph
        }

        private def buildStatement(stmt: ast.Statement, priority: Int, labels: Map[String, JumpTarget], here: Option[JumpTarget]): Graph = {
            implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry)
            def innerBlock: BlockInfo = block.inner(labelTargets = labels)

            stmt match {
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
                    val graph = buildStatement(body, priority + 1, labels + (name -> target), Some(target))
                    graph ~> merger

                case ast.BlockStatement(body) =>
                    buildInnerBlock(innerBlock, body, priority + 1)

                case ast.BreakStatement(targetLabel) =>
                    val target = resolveLoopTarget(targetLabel, labels)
                    buildJump(target.targetBlock, target.exit, priority)

                case ast.ContinueStatement(targetLabel) =>
                    val target = resolveLoopTarget(targetLabel, labels)
                    val loopEntry = target.entry.getOrElse(throw new BuildException(s"Can not continue the statement '${targetLabel.map(_.name).getOrElse("???")}'"))
                    buildJump(target.targetBlock, loopEntry, priority)

                case ast.ExpressionStatement(expr) =>
                    buildExressionStatement(expr, priority)

                case ast.TryStatement(tryBody, catchHandler, finallyHandler) =>
                    val finallyBuilder = finallyHandler map {
                        case ast.BlockStatement(finallyBody) =>
                            () => buildInnerBlock(block.inner(), finallyBody, priority + 1)
                    }


                    // build catch
                    val tryCatchGraph = catchHandler match {
                        case Some(ast.CatchClause(pattern, catchBody)) =>
                            ???
                            //val tryBlock = block.inner(catchEntry = Some(catchMerger), finalizer = finallyBuilder)
                            //val tryGraph = buildInnerBlock(tryBlock, tryBody.body, priority + 1)
                        case None =>
                            buildInnerBlock(block.inner(finalizer = finallyBuilder), tryBody.body, priority + 1)
                    }

                    // build finally
                    val finallyGraph = finallyBuilder map { _() } getOrElse EmptyGraph

                    return tryCatchGraph ~> finallyGraph

                case ast.IfStatement(test, consequent, alternate) =>
                    val testGraph = buildExpression(test, priority)
                    val thenGraph = buildInnerBlock(innerBlock, Seq(consequent), priority + 1)
                    val elseGraph = alternate.map(a => buildInnerBlock(innerBlock, Seq(a), priority + 1)).getOrElse(EmptyGraph)
                    val merger = new graph.MergeNode
                    val thenBegin = thenGraph.begin(merger)
                    val elseBegin = elseGraph.begin(merger)
                    val ifNode = new graph.CondJumpNode(thenBegin, elseBegin)
                    testGraph ~> ifNode
                    thenGraph ~> merger
                    elseGraph ~> merger // <- this might do nothing
                    Graph(testGraph.begin(ifNode), merger)

                case _: ast.DebuggerStatement =>
                    return EmptyGraph

                case _: ast.Directive =>
                    throw new AssertionError("Directives should not appear here")
            }
        }


        private def buildInnerBlock(block: BlockInfo, statements: Seq[ast.Statement], priority: Int): Graph = {
            val builder = new BlockBuilder(strict, block)
            builder.build(statements, priority)
        }

        def build(statements: Seq[ast.Statement], priority: Int): Graph = {
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
                    val stmtGraph = buildStatement(stmt, priority, block.labelTargets, None)
                    graph = graph ~> stmtGraph
            }
            done = true
            graph
        }
    }

    private[inferium] def buildGraph(scriptAst: ast.Program): Graph = {
        val builder = new BlockBuilder(false, new BlockInfo(None, Map.empty, None, None, None))
        builder.build(scriptAst.body collect { case stmt: ast.Statement => stmt }, 0)
    }

    def buildTemplate(scriptAst: ast.Program): Templates.Script =  new Templates.Script {
        override def instantiate(global: Entity): (Node, Node) = {
            (null, null)
        }
    }
}