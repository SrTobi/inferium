package inferium.dataflow
import escalima.ast
import inferium.Config.ConfigKey
import inferium.dataflow.graph._
import inferium.dataflow.graph.visitors.StackAnnotationVisitor
import inferium.lattice._

import scala.collection.mutable
import scala.util.Try


object GraphBuilder {

    private type VarMapping = Map[String, String]
    private case class JumpTarget(targetBlock: BlockInfo, entry: Option[MergeNode], exit: MergeNode)

    private def smooth(graphs: Seq[Graph]): Graph = graphs.fold(EmptyGraph){(from, to) => from ~> to }

    private class BlockInfo(val outer: Option[BlockInfo],
                            val labelTargets: Map[String, JumpTarget],
                            val loopTarget: Option[JumpTarget],
                            val catchEntry: Option[MergeNode],
                            val finalizer: Option[() => Graph]) {

        val depth: Int = outer.map(_.depth + 1).getOrElse(0)

        def inner(labelTargets: Map[String, JumpTarget] = this.labelTargets,
                  loopTarget: Option[JumpTarget] = this.loopTarget,
                  catchEntry: Option[MergeNode] = catchEntry,
                  finalizer: Option[() => Graph] = None): BlockInfo =
            new BlockInfo(Some(this), labelTargets, loopTarget, catchEntry, finalizer)
    }

    case class Config(bindLetAndConstToGlobal: Boolean)
    object Config extends inferium.Config.Section("GraphBuilder") {

        val bindLetAndConstToGlobal: ConfigKey[Boolean] = ConfigKey(false)
        val buildDebugNodes: ConfigKey[Boolean] = ConfigKey(false)


        implicit def configToGraphBuilder(config: inferium.Config): GraphBuilder.Config = GraphBuilder.Config(
            bindLetAndConstToGlobal = config(bindLetAndConstToGlobal)
        )
    }
}

class GraphBuilder(config: GraphBuilder.Config) {
    import GraphBuilder._

    private class FunctionBuilder(isScript: Boolean) {
        private[this] var done = false
        private val hoistables = mutable.Set.empty[String]
        private var hoistableGraph: Graph = EmptyGraph
        private var varRenameIdx = 1

        private class BlockBuilder(var strict: Boolean = false,
                                   block: BlockInfo,
                                   hoistingEnv: LexicalEnv) {

            private[this] var done = false

            private def buildJump(targetBlock: BlockInfo, targetNode: MergeNode, priority: Int, env: LexicalEnv): graph.JumpNode = {
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
                val jmp = new graph.JumpNode(unrollGraph.begin)(Node.Info(priority, targetBlock.catchEntry, env))
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

            private def addVarsToLexicalEnv(lexicalEnv: LexicalEnv, vars: Seq[(String, String)]): LexicalEnv = lexicalEnv.behavior match {
                case LexicalEnv.Behavior.Declarative(old) =>lexicalEnv.copy(behavior = LexicalEnv.Behavior.Declarative(old ++ vars))
                case _ => ???
            }

            private def buildLiteral(entity: Entity)(implicit info: Node.Info): Graph = {
                new graph.LiteralNode(entity)
            }

            private def buildExpression(expr: ast.Expression, priority: Int, env: LexicalEnv): Graph = {
                implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry, env)
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
                        new graph.LexicalReadNode(varName)
                }
            }

            private def buildExressionStatement(expr: ast.Expression, priority: Int, env: LexicalEnv): Graph = {
                implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry, env)

                val exprGraph = buildExpression(expr, priority, env)
                new PopNode() ~> exprGraph
            }

            private def buildPatternBinding(pattern: ast.Pattern, priority: Int, env: LexicalEnv): Graph = {
                implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry, env)

                pattern match {
                    case ast.Identifier(name) =>
                        new graph.LexicalWriteNode(name)

                    case _ => ???
                }
            }

            private def gatherBindingNamesFromPattern(pattern: ast.Pattern): Seq[String] = pattern match {
                case ast.Identifier(name) => Seq(name)
                case _ => ???
            }

            private def buildVarDeclaration(decl: ast.VariableDeclarator, priority: Int, env: LexicalEnv): Graph = decl match {
                case ast.VariableDeclarator(pattern, Some(init)) =>
                    val initGrah = buildExpression(init, priority, env)
                    val bindingGraph = buildPatternBinding(pattern, priority, env)
                    initGrah ~> bindingGraph
                case ast.VariableDeclarator(_, _) =>
                    // nothing to do for empty initializer
                    EmptyGraph
            }

            private def buildStatement(stmt: ast.Statement,
                                       priority: Int,
                                       labels: Map[String, JumpTarget],
                                       here: Option[JumpTarget],
                                       env: LexicalEnv): (Graph, LexicalEnv) = {
                implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry, env)
                def newInnerBlockEnv(): LexicalEnv = new LexicalEnv(Some(env), false, LexicalEnv.Behavior.Declarative(Map.empty))
                def innerBlock: BlockInfo = block.inner(labelTargets = labels)

                var newEnv = env
                val result: Graph = stmt match {
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
                        val (graph, resultEnv) = buildStatement(body, priority + 1, labels + (name -> target), Some(target), env)
                        newEnv = resultEnv
                        graph ~> merger

                    case ast.BlockStatement(body) =>
                        buildInnerBlock(innerBlock, body, priority + 1, newInnerBlockEnv())

                    case ast.BreakStatement(targetLabel) =>
                        val target = resolveLoopTarget(targetLabel, labels)
                        buildJump(target.targetBlock, target.exit, priority, env)

                    case ast.ContinueStatement(targetLabel) =>
                        val target = resolveLoopTarget(targetLabel, labels)
                        val loopEntry = target.entry.getOrElse(throw new BuildException(s"Can not continue the statement '${targetLabel.map(_.name).getOrElse("???")}'"))
                        buildJump(target.targetBlock, loopEntry, priority, env)

                    case ast.ExpressionStatement(expr) =>
                        buildExressionStatement(expr, priority, env)

                    case ast.TryStatement(tryBody, catchHandler, finallyHandler) =>
                        val finallyBuilder = finallyHandler map {
                            case ast.BlockStatement(finallyBody) =>
                                () => buildInnerBlock(block.inner(), finallyBody, priority + 1, newInnerBlockEnv())
                        }


                        // build catch
                        val tryCatchGraph = catchHandler match {
                            case Some(ast.CatchClause(pattern, catchBody)) =>
                                val catchMerger = new graph.MergeNode(isCatchMerger = true)

                                val tryBlock = block.inner(catchEntry = Some(catchMerger))
                                val tryGraph = buildInnerBlock(tryBlock, tryBody.body, priority + 1, newInnerBlockEnv())

                                val catchEnv = newInnerBlockEnv()
                                val bindingGraph = buildPatternBinding(pattern, priority, catchEnv)
                                val catchBlock = block.inner(finalizer = finallyBuilder)
                                val catchGraph = buildInnerBlock(catchBlock, catchBody.body, priority + 1, catchEnv)

                                val afterMerger = new graph.MergeNode
                                // we need the jump or we wont find the catch branch with visitors
                                val jmpInfo = info.copy(priority = info.priority + 1)
                                tryGraph ~> new graph.JumpNode(afterMerger)(jmpInfo) ~> catchMerger ~> bindingGraph ~> catchGraph ~> afterMerger

                            case None =>
                                buildInnerBlock(block.inner(finalizer = finallyBuilder), tryBody.body, priority + 1, newInnerBlockEnv())
                        }

                        // build finally
                        val finallyGraph = finallyBuilder map { _() } getOrElse EmptyGraph

                        tryCatchGraph ~> finallyGraph

                    case ast.IfStatement(test, consequent, alternate) =>
                        val testGraph = buildExpression(test, priority, env)
                        val thenGraph = buildInnerBlock(innerBlock, Seq(consequent), priority + 1, newInnerBlockEnv())
                        val elseGraph = alternate.map(a => buildInnerBlock(innerBlock, Seq(a), priority + 1, newInnerBlockEnv())).getOrElse(EmptyGraph)
                        val merger = new graph.MergeNode
                        val thenBegin = thenGraph.begin(merger)
                        val elseBegin = elseGraph.begin(merger)
                        val ifNode = new graph.CondJumpNode(thenBegin, elseBegin)
                        testGraph ~> ifNode
                        thenGraph ~> merger
                        elseGraph ~> merger // <- this might do nothing
                        Graph(testGraph.begin(ifNode), merger)

                    case ast.VariableDeclaration(decls, kind) =>

                        lazy val hoistingInfo = info.copy(lexicalEnv = hoistingEnv)
                        val bindingNames = decls map { _.id } flatMap { gatherBindingNamesFromPattern }
                        newEnv = if (kind == ast.VariableDeclarationKind.`var`) {
                            hoistables ++= bindingNames
                            if (isScript) {
                                // if we are not in a function `var statements` directly modify the global object
                                // so we have to write undefined into it even if there is no initializer
                                // because of enumeration
                                val hoistingGraphs = bindingNames map { name => buildLiteral(UndefinedValue)(hoistingInfo) ~> new LexicalWriteNode(name)(hoistingInfo) }
                                hoistableGraph ~>= smooth(hoistingGraphs)
                            }
                            env
                        } else {
                            def rename(name: String): String = name + {varRenameIdx += 1; varRenameIdx}
                            addVarsToLexicalEnv(env, bindingNames map { n => (n, rename(n)) })
                        }

                        val declGraphs = decls map { buildVarDeclaration(_, priority, newEnv) }

                        smooth(declGraphs)

                    case _: ast.EmptyStatement =>
                        EmptyGraph

                    case _: ast.DebuggerStatement =>
                        EmptyGraph

                    case _: ast.Directive =>
                        throw new AssertionError("Directives should not appear here")
                }

                return (result, newEnv)
            }


            private def buildInnerBlock(block: BlockInfo, statements: Seq[ast.Statement], priority: Int, initialEnv: LexicalEnv): Graph = {
                val builder = new BlockBuilder(strict, block, hoistingEnv)
                builder.build(statements, priority, initialEnv)
            }

            def build(statements: Seq[ast.Statement], priority: Int, initialEnv: LexicalEnv): Graph = {
                assert(!done)

                // check that directives only appear at the beginning
                assert(statements.span(_.isInstanceOf[ast.Directive])._2.forall(!_.isInstanceOf[ast.Directive]))

                var graph: Graph = EmptyGraph
                var visitedNonDirective = false
                var env = initialEnv

                statements foreach {
                    case ast.Directive(_, directive) =>
                        assert(!visitedNonDirective)
                        if (directive == "use strict;") {
                            strict = true
                        }

                    case stmt =>
                        visitedNonDirective = true
                        val (stmtGraph, resultEnv) = buildStatement(stmt, priority, block.labelTargets, None, env)
                        env = resultEnv
                        graph = graph ~> stmtGraph
                }
                done = true
                graph
            }
        }


        def build(program: ast.Program, strict: Boolean): ScriptGraph = {
            val priority = 0
            assert(!done)
            val globalEnv = new LexicalEnv(None, true, LexicalEnv.Behavior.Hoisted(hoistables))
            val firstVarsEnv = new LexicalEnv(Some(globalEnv), true, LexicalEnv.Behavior.Declarative(Map.empty))
            val builder = new BlockBuilder(false, new BlockInfo(None, Map.empty, None, None, None), globalEnv)
            val graph = builder.build(program.body collect { case stmt: ast.Statement => stmt }, priority, firstVarsEnv)
            done = true
            implicit val info: Node.Info = Node.Info(priority, None, globalEnv)
            val endNode = new EndNode
            val scriptGraph = hoistableGraph ~> new PushLexicalFrame() ~> graph ~> endNode
            ScriptGraph(scriptGraph.begin(endNode), endNode)
        }
    }

    def buildTemplate(scriptAst: ast.Program): Templates.Script =  new Templates.Script {
        override def instantiate(): ScriptGraph = {
            val builder = new FunctionBuilder(isScript = false)
            val graph = builder.build(scriptAst, strict = false)
            new StackAnnotationVisitor().start(graph)
            graph
        }
    }
}