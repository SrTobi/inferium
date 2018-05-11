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

    case class Config(bindLetAndConstToGlobal: Boolean, buildDebugNodes: Boolean, debugObjectName: String)
    object Config extends inferium.Config.Section("GraphBuilder") {

        val bindLetAndConstToGlobal: ConfigKey[Boolean] = ConfigKey(false)
        val buildDebugNodes: ConfigKey[Boolean] = ConfigKey(false)
        val debugObjectName: ConfigKey[String] = ConfigKey("debug")


        implicit def configToGraphBuilder(config: inferium.Config): GraphBuilder.Config = GraphBuilder.Config(
            bindLetAndConstToGlobal = config(bindLetAndConstToGlobal),
            buildDebugNodes = config(buildDebugNodes),
            debugObjectName = config(debugObjectName)
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

            private def parseAbstractDebugLiteral(name: String): Primitive = name match {
                case "never" => NeverValue
                case "string" => StringValue
                case "boolean" => BoolValue
                case "number" => NumberValue
            }

            private def parseDebugLiteral(expr: ast.SpreadableExpression): Either[Primitive, String] = expr match {
                case ast.BooleanLiteral(value) =>
                    Left(BoolValue(value))

                case ast.StringLiteral(value) =>
                    Left(StringValue(value))

                case num: ast.NumberLiteral =>
                    val value = Try(num.raw.toInt).map(SpecificNumberValue).getOrElse(NumberValue)
                    Left(value)

                case _: ast.NullLiteral =>
                    Left(NullValue)

                case ast.Identifier("undefined") =>
                    Left(UndefinedValue)

                case ast.Identifier(varName) =>
                    Right(varName)

                case ast.MemberExpression(ast.Identifier(base), ast.Identifier(name), false) if base == config.debugObjectName =>
                    Left(parseAbstractDebugLiteral(name))
            }

            private def parseDebugExpression(expr: ast.Expression, needExpression: Boolean, needSubject: Boolean = false, hadExpression: Boolean = false): Option[(Seq[DebugNode.Operation], Option[ast.Expression])] = {
                expr match {
                        // base
                    case ast.Identifier(base) if base == config.debugObjectName =>
                        if (needExpression || needSubject) {
                            throw new BuildException("Debug expression needed an expression")
                        } else {
                            Some(Seq(DebugNode.CheckLiveCode), None)
                        }

                    case ast.MemberExpression(ast.Identifier(base), ast.Identifier("ans"), false) if base == config.debugObjectName =>
                        if (!needSubject) {
                            throw new BuildException("ans debug expression needs tester")
                        } else if (needExpression) {
                            throw new BuildException("An expression is needed. ans can only be used in debug statements")
                        } else if (hadExpression) {
                            throw new BuildException("Debug can have only one main expression")
                        } else {
                            Some(Seq(DebugNode.CheckLiveCode), None)
                        }

                    case ast.CallExpression(ast.Identifier(base), Seq(innerExpr: ast.Expression)) if base == config.debugObjectName =>
                        if (hadExpression) {
                            throw new BuildException("Debug can have only one main expression")
                        } else {
                            Some(Seq(DebugNode.CheckLiveCode), Some(innerExpr))
                        }

                        // liveCode
                    case ast.CallExpression(ast.MemberExpression(callee: ast.Expression, ast.Identifier("liveCode"), false), Seq()) =>
                        parseDebugExpression(callee, needExpression, needSubject, hadExpression) map {
                            case (ops, innerExpr) => (ops :+ DebugNode.CheckLiveCode, innerExpr)
                        }

                        // deadCode
                    case ast.CallExpression(ast.MemberExpression(callee: ast.Expression, ast.Identifier("deadCode"), false), Seq()) =>
                        parseDebugExpression(callee, needExpression, needSubject, hadExpression) map {
                            case (ops, innerExpr) => (ops :+ DebugNode.CheckDeadCode, innerExpr)
                        }

                    case ast.CallExpression(ast.MemberExpression(source: ast.Expression, ast.Identifier("isOneOf"), false), args) =>
                        parseDebugExpression(source, needExpression, needSubject = true) map {
                            case (ops, innerExpr) =>
                                val op = DebugNode.OneOf(args map parseDebugLiteral)
                                (ops :+ op, innerExpr)
                        }

                    case ast.CallExpression(ast.MemberExpression(source: ast.Expression, ast.Identifier("print"), false), Seq()) =>
                        parseDebugExpression(source, needExpression, needSubject = true) map {
                            case (ops, innerExpr) =>
                                (ops :+ DebugNode.PrintExpr, innerExpr)
                        }

                    case ast.MemberExpression(source: ast.Expression, member, _) =>
                        // do not throw the exception if we check for an debug STATEMENT
                        if (needSubject) {
                            parseDebugExpression(source, needExpression = false) foreach {
                                _ => throw new BuildException(s"Member $member is not a known debug function")
                            }
                        }

                        None
                    case _ =>
                        None
                }
            }

            private object AbstractDebugLiteral {
                def unapply(arg: ast.Expression): Option[Primitive] = if (!config.buildDebugNodes) None else arg match {
                    case ast.MemberExpression(ast.Identifier(base), ast.Identifier(lit), false) if base == config.debugObjectName =>
                        Try(parseAbstractDebugLiteral(lit)).toOption
                    case _ =>
                        None
                }
            }

            private object DebugExpression {
                def unapply(expr: ast.Expression): Option[(Seq[DebugNode.Operation], Option[ast.Expression])] = if (!config.buildDebugNodes) None else {
                    parseDebugExpression(expr, needExpression = true)
                }
            }

            private object DebugStatement {
                def unapply(arg: ast.Statement): Option[(Seq[DebugNode.Operation], Option[ast.Expression])] = if (!config.buildDebugNodes) None else arg match {
                    case ast.ExpressionStatement(expr) =>
                        parseDebugExpression(expr, needExpression = false)
                    case _ =>
                        None
                }
            }

            private def buildExpression(expr: ast.Expression, priority: Int, env: LexicalEnv): Graph = BuildException.enrich(expr) {
                implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry, env)
                expr match {
                    case AbstractDebugLiteral(literal) =>
                        buildLiteral(literal)

                    case DebugExpression(ops, innerExprOpt) =>
                        val innerExpr = innerExprOpt getOrElse (throw new BuildException("debug within expressions need a base expression"))
                        val exprGraph = buildExpression(innerExpr, priority, env)
                        exprGraph ~> new DebugNode(ops)

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
                        if (config.buildDebugNodes && config.debugObjectName == varName) {
                            throw new BuildException(s"The identifier '${config.debugObjectName}' may solely be used in debug expressions!")
                        }
                        new graph.LexicalReadNode(varName)

                    case ast.MemberExpression(obj: ast.Expression, ast.Identifier(propertyName), false) =>
                        // read static member (base.propertyName)
                        val baseGraph = buildExpression(obj, priority, env)
                        baseGraph ~> new graph.PropertyReadNode(propertyName)

                    case ast.AssignmentExpression(op, left: ast.Pattern, right) =>
                        buildAssignment(left, Some(right), priority, env, pushWrittenValueToStack = true)

                    case ast.ObjectExpression(properties) =>
                        val propertyInitGraphs = properties map {
                            case ast.SpreadElement(arg) =>
                                ???

                            case ast.Property(key, value, kind, isMethod, isShorthand, isComputed) =>
                                if (kind != ast.PropertyKind.init) ???
                                if (isMethod) ???

                                def valueGraph: Graph = buildExpression(value, priority, env)

                                (isComputed, key) match {
                                    case (false, ast.Identifier(name)) =>
                                        valueGraph ~> new graph.PropertyWriteNode(name) ~> new graph.PopNode
                                    case _ =>
                                        // todo: write dynamic property
                                        buildExpression(key, priority, env) ~> ???
                                }
                        }

                        val dupGraph: Graph = if (propertyInitGraphs.isEmpty) EmptyGraph else new graph.DupNode(propertyInitGraphs.length)
                        val propertyInitGraph = propertyInitGraphs.foldLeft[Graph](EmptyGraph) { _ ~> _}

                        new graph.AllocateObjectNode ~> dupGraph ~> propertyInitGraph//propertyInitGraph
                }
            }

            private def buildAssignment(pattern: ast.Pattern, init: Option[ast.Expression], priority: Int, env: LexicalEnv, pushWrittenValueToStack: Boolean): Graph = {
                implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry, env)

                def buildExpr(expr: ast.Expression) = buildExpression(expr, priority, env)
                def initGraph: Graph = init map { buildExpr } getOrElse { new graph.LiteralNode(UndefinedValue) }
                def consumeResult: Graph = if (pushWrittenValueToStack) EmptyGraph else { new graph.PopNode }

                val assignmentGraph = pattern match {
                    case ast.MemberExpression(obj: ast.Expression, ast.Identifier(propertyName), false) =>
                        val objGraph = buildExpr(obj)

                        objGraph ~> initGraph ~> new graph.PropertyWriteNode(propertyName)

                    case _ =>
                        initGraph ~> buildPatternBinding(pattern, priority, env, pushWrittenValueToStack = true)
                }

                assignmentGraph ~> consumeResult
            }

            private def buildPatternBinding(pattern: ast.Pattern, priority: Int, env: LexicalEnv, pushWrittenValueToStack: Boolean): Graph = {
                implicit lazy val info: Node.Info = Node.Info(priority, block.catchEntry, env)

                def consumeResult: Graph = if (pushWrittenValueToStack) EmptyGraph else { new graph.PopNode }

                val bindingGraph = pattern match {
                    case ast.Identifier(name) =>
                        new graph.LexicalWriteNode(name)

                    case _ => ???
                }

                bindingGraph ~> consumeResult
            }

            private def gatherBindingNamesFromPattern(pattern: ast.Pattern): Seq[String] = pattern match {
                case ast.Identifier(name) => Seq(name)
                case _ => ???
            }

            private def buildVarDeclaration(decl: ast.VariableDeclarator, priority: Int, env: LexicalEnv): Graph = decl match {
                case ast.VariableDeclarator(pattern, init@Some(_)) =>
                    buildAssignment(pattern, init, priority, env, pushWrittenValueToStack = false)
                case ast.VariableDeclarator(_, _) =>
                    // nothing to do for empty initializer
                    EmptyGraph
            }

            private def buildStatement(stmt: ast.Statement,
                                       priority: Int,
                                       labels: Map[String, JumpTarget],
                                       here: Option[JumpTarget],
                                       env: LexicalEnv): (Graph, LexicalEnv) =  BuildException.enrich(stmt) {
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
                                val merger = new MergeNode
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

                    case DebugStatement(ops, innerExprOpt) =>
                        def debugNode = new DebugNode(ops)
                        innerExprOpt match {
                            case Some(expr) =>
                                val exprGraph = buildExpression(expr, priority, env)
                                exprGraph ~> debugNode ~> new PopNode()
                            case None =>
                                debugNode
                        }

                    case ast.ExpressionStatement(expr) =>
                        val exprGraph = buildExpression(expr, priority, env)
                        new PopNode() ~> exprGraph

                    case ast.TryStatement(tryBody, catchHandler, finallyHandler) =>
                        val finallyBuilder = finallyHandler map {
                            case ast.BlockStatement(finallyBody) =>
                                () => buildInnerBlock(block.inner(), finallyBody, priority + 1, newInnerBlockEnv())
                        }


                        // build catch
                        val tryCatchGraph = catchHandler match {
                            case Some(ast.CatchClause(pattern, catchBody)) =>
                                val catchMerger = new graph.MergeNode(isCatchMerger = true)(info.copy(priority = priority + 1))

                                val tryBlock = block.inner(catchEntry = Some(catchMerger))
                                val tryGraph = buildInnerBlock(tryBlock, tryBody.body, priority + 2, newInnerBlockEnv())

                                val catchEnv = newInnerBlockEnv()
                                val expBindingGraph = buildPatternBinding(pattern, priority + 1, catchEnv, pushWrittenValueToStack = false)
                                val catchBlock = block.inner(finalizer = finallyBuilder)
                                val catchGraph = buildInnerBlock(catchBlock, catchBody.body, priority + 1, catchEnv)

                                val afterMerger = new graph.MergeNode
                                // we need the jump or we wont find the catch branch with visitors
                                val jmpInfo = info.copy(priority = info.priority + 2)
                                tryGraph ~> new graph.JumpNode(afterMerger)(jmpInfo) ~> catchMerger ~> expBindingGraph ~> catchGraph ~> afterMerger

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

                    case ast.WhileStatement(test, body) =>
                        val testerInfo = Node.Info(priority + 1, block.catchEntry, env)
                        val loopMerger = new graph.MergeNode(fixpoint = true)(testerInfo)
                        val afterMerger = new graph.MergeNode
                        val testGraph = buildExpression(test, priority + 1, env)
                        val bodyGraph = buildInnerBlock(innerBlock, Seq(body), priority + 2, newInnerBlockEnv())
                        val cond = new graph.CondJumpNode(bodyGraph.begin(loopMerger), afterMerger)

                        loopMerger ~> testGraph ~> cond
                        bodyGraph ~> loopMerger
                        Graph(loopMerger, afterMerger)

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

                var bodyGraph: Graph = EmptyGraph
                var visitedNonDirective = false
                var env = initialEnv

                statements foreach {
                    case ast.Directive(_, directive) =>
                        assert(!visitedNonDirective)

                        if (directive == "use strict") {
                            strict = true
                        }

                        // build expression
                        implicit val info: Node.Info = Node.Info(priority, block.catchEntry, env, None)
                        val stringExpr = new graph.PopNode ~> buildLiteral(StringValue(directive))
                        bodyGraph = bodyGraph ~> stringExpr

                    case stmt =>
                        visitedNonDirective = true
                        val (stmtGraph, resultEnv) = buildStatement(stmt, priority, block.labelTargets, None, env)
                        env = resultEnv
                        bodyGraph = bodyGraph ~> stmtGraph
                }
                done = true
                bodyGraph
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