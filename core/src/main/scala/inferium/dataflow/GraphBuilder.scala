package inferium.dataflow
import escalima.ast
import inferium.Config.ConfigKey
import inferium.dataflow.calls.{CallInstance, InlinedCallInstance}
import inferium.dataflow.graph.MergeNode.MergeType
import inferium.dataflow.graph._
import inferium.dataflow.graph.visitors.StackAnnotationVisitor
import inferium.lattice._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
import scala.language.implicitConversions


object GraphBuilder {

    private type VarMapping = Map[String, String]
    private class JumpTarget(val targetBlock: BlockInfo, val exit: MergeNode) {
        private var _entry: Option[MergeNode] = None
        def entry: Option[MergeNode] = _entry
        def entry_=(node: MergeNode): Unit = {
            assert(_entry.isEmpty)
            _entry = Some(node)
        }
    }

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

    private class FunctionBuilder(val isTopLevel: Boolean,
                                  val functionFrame: Node.CallFrame,
                                  val functionPriority: Int) {
        private[this] var done = false
        private val hoistables = mutable.Set.empty[String]
        private var hoistableGraph: Graph = EmptyGraph
        private var hoistingNodeInfo: Node.Info = _
        private var varRenameIdx = 1
        private var returnBlockInfo: BlockInfo = _
        private var returnMergeNode: MergeNode = _


        private def makeLocalNameUnique(name: String): String = name + "~" + {varRenameIdx += 1; varRenameIdx}

        private def gatherBindingNamesFromPattern(pattern: ast.Pattern): Seq[String] = pattern match {
            case ast.Identifier(name) => Seq(name)
            case _ => ???
        }

        private def popToResult(g: Graph)(implicit info: Node.Info): Graph = {
            if (isTopLevel) {
                new graph.PopNode ~> g
            } else {
                g ~> new graph.PopNode
            }
        }

        private class BlockBuilder(var strict: Boolean = false,
                                   block: BlockInfo,
                                   hoistingEnv: LexicalEnv,
                                   blockLexicalEnv: LexicalEnv,
                                   blockPriority: Int) {

            private var blockHoistableGraph: Graph = EmptyGraph
            //private val blockHoistables = if (isTopLevel) null else mutable.Map.empty[String, String]
            //private val blockHoistingEnv = if (isTopLevel) hoistingEnv else new LexicalEnv(Some(blockLexicalEnv), false, LexicalEnv.Behavior.BlockHoisted(blockHoistables))
            private val blockHoistingNodeInfo = Node.Info(blockPriority, blockLexicalEnv, functionFrame, block.catchEntry)
            //private def initialBlockEnv: LexicalEnv = if (isTopLevel) blockLexicalEnv else blockHoistingEnv
            private[this] var done = false

            def makeBlockInfo(priority: Int, lexicalEnv: LexicalEnv): Node.Info = Node.Info(priority, lexicalEnv, functionFrame, block.catchEntry)

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
                val jmp = new graph.JumpNode(unrollGraph.begin)(makeBlockInfo(priority, env))
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

            @tailrec
            private def addVarsToLexicalEnv(lexicalEnv: LexicalEnv, vars: Seq[(String, String)]): LexicalEnv = lexicalEnv.behavior match {
                case LexicalEnv.Behavior.Declarative(old) => lexicalEnv.copy(behavior = LexicalEnv.Behavior.Declarative(old ++ vars))
                case _ =>
                    val outer = lexicalEnv.outer.getOrElse(throw new AssertionError("Expected to find a declarative lexical environment"))
                    addVarsToLexicalEnv(outer, vars)
            }

            private def buildLiteral(entity: Entity)(implicit info: Node.Info): Graph = {
                new graph.LiteralNode(entity)
            }

            private def parseAbstractDebugLiteral(name: String): Entity = name match {
                case "never" => NeverValue
                case "string" => StringValue
                case "boolean" => BoolValue
                case "number" => NumberValue
                case "any" => AnyEntity
            }

            private def parseDebugLiteral(expr: ast.SpreadableExpression): Either[Entity, String] = expr match {
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
                            case (ops, innerExpr) => (ops.filter(_ != DebugNode.CheckLiveCode) :+ DebugNode.CheckDeadCode, innerExpr)
                        }

                    case ast.CallExpression(ast.MemberExpression(callee: ast.Expression, ast.Identifier("mightBeDead"), false), Seq()) =>
                        parseDebugExpression(callee, needExpression, needSubject, hadExpression) map {
                            case (ops, innerExpr) => (ops.filter(_ != DebugNode.CheckLiveCode), innerExpr)
                        }

                    case ast.CallExpression(ast.MemberExpression(source: ast.Expression, ast.Identifier("is"), false), args) =>
                        parseDebugExpression(source, needExpression, needSubject = true) map {
                            case (ops, innerExpr) =>
                                val op = DebugNode.Is(args map parseDebugLiteral)
                                (ops :+ op, innerExpr)
                        }

                    case ast.CallExpression(ast.MemberExpression(source: ast.Expression, ast.Identifier("print"), false), args) =>
                        val name = args match {
                            case Seq(ast.StringLiteral(value)) =>
                                value
                            case Seq() =>
                                "unknown"
                            case _ =>
                                throw new BuildException("debug.print() can only take one string argument")
                        }
                        parseDebugExpression(source, needExpression, needSubject = true) map {
                            case (ops, innerExpr) =>
                                (ops :+ DebugNode.PrintExpr(name), innerExpr)
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
                def unapply(arg: ast.Expression): Option[Entity] = if (!config.buildDebugNodes) None else arg match {
                    case ast.MemberExpression(ast.Identifier(base), ast.Identifier(lit), false) if base == config.debugObjectName =>
                        Some(parseAbstractDebugLiteral(lit))
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
                implicit lazy val info: Node.Info = makeBlockInfo(priority, env)
                def buildExpression(expr: ast.Expression, priority: Int = priority, env: LexicalEnv = env): Graph = this.buildExpression(expr, priority, env)

                expr match {
                    case AbstractDebugLiteral(literal) =>
                        buildLiteral(literal)

                    case DebugExpression(ops, innerExprOpt) =>
                        val innerExpr = innerExprOpt getOrElse (throw new BuildException("debug within expressions need a base expression"))
                        val exprGraph = buildExpression(innerExpr)
                        exprGraph ~> new DebugNode(ops, expr.loc.map(_.start.line))

                    case ast.CallExpression(ast.MemberExpression(ast.Identifier("debug"), ast.Identifier("squash"), false), args) if config.buildDebugNodes =>
                        if (args.length < 2) {
                            throw new BuildException(s"debug.squash needs at least 2 arguments")
                        }

                        val argGraph = Graph.concat(args map { _.asInstanceOf[ast.Expression] } map { buildExpression(_) })
                        argGraph ~> new graph.DebugSquashNode(args.length)

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

                    case _: ast.ThisExpression =>
                        new graph.PushThisNode()

                    case memberExpr: ast.MemberExpression =>
                        buildMemberAccess(memberExpr, priority, env)

                    case ast.AssignmentExpression(op, left: ast.Pattern, right) =>
                        buildAssignment(left, Some(right), priority, env, pushWrittenValueToStack = true)

                    case ast.ObjectExpression(properties) =>
                        val propertyInitGraphs = properties map {
                            case ast.SpreadElement(arg) =>
                                ???

                            case ast.Property(key, value, kind, isMethod, isShorthand, isComputed) =>
                                if (kind != ast.PropertyKind.init) ???

                                val valueGraph: Graph = buildExpression(value)

                                buildPropertyWrite(key, isComputed, valueGraph, priority, env) ~> new graph.PopNode
                        }

                        val dupGraph: Graph = if (propertyInitGraphs.isEmpty) EmptyGraph else new graph.DupNode(propertyInitGraphs.length)
                        val propertyInitGraph = Graph.concat(propertyInitGraphs)

                        new graph.AllocateObjectNode ~> dupGraph ~> propertyInitGraph//propertyInitGraph

                    case expr: ast.FunctionExpression =>
                        buildFunction(expr)

                    case ast.ArrowFunctionExpression(id, params, body, isAsync) =>
                        val tmpl = FunctionTemplate(id map { _.name }, params, body, env)
                        buildFunction(tmpl, isGenerator = false, isAsync, captureThis = true)

                    case ast.CallExpression(callee, arguments) =>
                        // Stack for a function call: this, func, args...
                        val (thisAndFuncGraph, hasThis) = callee match {
                            case sup: ast.Super =>
                                // todo: implement super call
                                ???
                            case memberAccess: ast.MemberExpression =>
                                val accessGraph = buildMemberAccess(memberAccess, priority, env, needThis = true)
                                (accessGraph, true)

                            case other: ast.Expression =>
                                val funcGraph = buildExpression(other)
                                (funcGraph, false)
                        }

                        val argGraphs = arguments map {
                            case ast.SpreadElement(element) =>
                                element
                            case expr: ast.Expression =>
                                expr
                        } map {
                            buildExpression(_)
                        }

                        val spreadArguments = arguments map { _.isInstanceOf[ast.SpreadElement] }

                        thisAndFuncGraph ~> Graph.concat(argGraphs) ~> new graph.CallNode(hasThis, spreadArguments)

                    case ast.ConditionalExpression(test, consequence, alternate) =>
                        val testGraph = buildExpression(test)
                        val thenGraph = buildExpression(consequence, priority + 1)
                        val elseGraph = buildExpression(alternate, priority + 1)
                        val merger = new graph.MergeNode
                        // The testGraph, thenGraph and the elseGraph can not be empty!
                        assert(testGraph != EmptyGraph)
                        assert(thenGraph != EmptyGraph)
                        assert(elseGraph != EmptyGraph)
                        val ifNode = new graph.CondJumpNode(thenGraph.begin, elseGraph.begin)
                        testGraph ~> ifNode
                        thenGraph ~> merger
                        elseGraph ~> merger
                        Graph(testGraph.begin, merger)

                    case ast.SequenceExpression(exprs) =>
                        val exprsIt = exprs.iterator

                        val graphs = exprsIt map {
                            expr =>
                                val lastExpr = !exprsIt.hasNext
                                val exprGraph = buildExpression(expr)

                                if (lastExpr) {
                                    exprGraph
                                } else {
                                    exprGraph ~> new graph.PopNode
                                }
                        }

                        Graph.concat(graphs)
                }
            }

            private def buildMemberAccess(node: ast.MemberExpression, priority: Int, env: LexicalEnv, needThis: Boolean = false)(implicit nodeInfo: Node.Info): Graph = {
                lazy val dupGraph: Graph = if (needThis) new graph.DupNode(1) else EmptyGraph
                node match {
                    case ast.MemberExpression(obj: ast.Expression, property, computed) =>
                        // read static member (base.propertyName)
                        val baseGraph = buildExpression(obj, priority, env)
                        baseGraph ~> dupGraph ~> buildPropertyRead(property, computed, nodeInfo.priority, nodeInfo.lexicalEnv)

                }
            }

            private def buildFunction(tmpl: FunctionTemplate, isGenerator: Boolean, isAsync: Boolean, captureThis: Boolean)(implicit nodeInfo: Node.Info): Graph = {
                if (isGenerator) {
                    throw new BuildException("Inferium doesn't support generators at the moment!")
                }

                if (isAsync) {
                    throw new BuildException("Inferium doesn't support async functions at the moment!")
                }

                new graph.AllocateFunctionNode(tmpl, captureThis)
            }

            private def buildFunction(func: ast.Function)(implicit nodeInfo: Node.Info): Graph = {
                val ast.Function(id, params, body, isGenerator, isAsync) = func
                val tmpl = FunctionTemplate(id map { _.name }, params, body, nodeInfo.lexicalEnv)
                buildFunction(tmpl, isGenerator, isAsync, captureThis = false)
            }

            object ConstantProperty {
                def unapply(arg: ast.Expression): Option[String] = {
                    implicit def stringToOption(str: String): Some[String] = Some(str)
                    arg match {
                        case ast.BooleanLiteral(value) => value.toString
                        case ast.StringLiteral(value) => value
                        case num: ast.NumberLiteral => num.raw
                        case _: ast.NullLiteral => "null"
                        case ast.Identifier("undefined") => "undefined"
                        case _ => None
                    }
                }
            }

            private def buildPropertyRead(property: ast.Expression, computed: Boolean, priority: Int, lexicalEnv: LexicalEnv): Graph = {
                implicit val info: Node.Info = makeBlockInfo(priority, lexicalEnv)
                property match {
                    case ConstantProperty(propertyName) =>
                        new PropertyReadNode(propertyName)
                    case ast.Identifier(propertyName) if !computed =>
                        new PropertyReadNode(propertyName)
                    case expr =>
                        assert(computed)
                        val propertyGraph = buildExpression(expr, priority, lexicalEnv)
                        propertyGraph ~> new PropertyDynamicReadNode
                }
            }


            private def buildPropertyWrite(property: ast.Expression, computed: Boolean, valueGraph: Graph, priority: Int, lexicalEnv: LexicalEnv): Graph = {
                implicit val info: Node.Info = makeBlockInfo(priority, lexicalEnv)
                property match {
                    case ConstantProperty(propertyName) =>
                        valueGraph ~> new PropertyWriteNode(propertyName)
                    case ast.Identifier(propertyName) if !computed =>
                        valueGraph ~> new PropertyWriteNode(propertyName)
                    case expr =>
                        assert(computed)
                        val propertyGraph = buildExpression(expr, priority, lexicalEnv)
                        propertyGraph ~> valueGraph ~> new PropertyDynamicWriteNode

                }
            }

            private def buildAssignment(pattern: ast.Pattern, init: Option[ast.Expression], priority: Int, env: LexicalEnv, pushWrittenValueToStack: Boolean): Graph = {
                implicit lazy val info: Node.Info = makeBlockInfo(priority, env)

                def buildExpr(expr: ast.Expression) = buildExpression(expr, priority, env)
                def initGraph: Graph = init map { buildExpr } getOrElse { new graph.LiteralNode(UndefinedValue) }
                def consumeResult: Graph = if (pushWrittenValueToStack) EmptyGraph else { new graph.PopNode }

                val assignmentGraph = pattern match {
                    case ast.MemberExpression(obj: ast.Expression, property, computed) =>
                        val objGraph = buildExpr(obj)

                        objGraph ~> buildPropertyWrite(property, computed, initGraph, priority, env)

                    case _ =>
                        initGraph ~> buildPatternBinding(pattern, priority, env, pushWrittenValueToStack = true)
                }

                assignmentGraph ~> consumeResult
            }

            private def buildPatternBinding(pattern: ast.Pattern, priority: Int, env: LexicalEnv, pushWrittenValueToStack: Boolean): Graph = {
                implicit lazy val info: Node.Info = makeBlockInfo(priority, env)

                def consumeResult: Graph = if (pushWrittenValueToStack) EmptyGraph else { new graph.PopNode }

                val bindingGraph = pattern match {
                    case ast.Identifier(name) =>
                        new graph.LexicalWriteNode(name)

                    case _ => ???
                }

                bindingGraph ~> consumeResult
            }

            private def buildVarDeclarationAssignment(decl: ast.VariableDeclarator, priority: Int, env: LexicalEnv): Graph = decl match {
                case ast.VariableDeclarator(pattern, init@Some(_)) =>
                    buildAssignment(pattern, init, priority, env, pushWrittenValueToStack = false)
                case ast.VariableDeclarator(_, _) =>
                    // nothing to do for empty initializer
                    EmptyGraph
            }

            private def buildStatement(stmt: ast.Statement,
                                       priority: Int,
                                       labels: Map[String, JumpTarget],
                                       hereOpt: Option[JumpTarget],
                                       env: LexicalEnv): (Graph, LexicalEnv) =  BuildException.enrich(stmt) {
                implicit lazy val info: Node.Info = Node.Info(priority, env, functionFrame, block.catchEntry)
                def newInnerBlockEnv(baseEnv: LexicalEnv = env): LexicalEnv = new LexicalEnv(Some(baseEnv), false, LexicalEnv.Behavior.Declarative(Map.empty))
                lazy val innerBlock: BlockInfo = block.inner(labelTargets = labels)

                // gets or builds the facilities for jumps (continue/break incl. labels)
                // returns the jump target for the current statement
                lazy val here = hereOpt match {
                    case Some(hereTarget) =>
                        hereTarget
                    case None =>
                        val merger = new MergeNode
                        new JumpTarget(block, merger)
                }

                var newEnv = env

                def reserveHoistedName(name: String): Unit = {
                    // if we are not in a function `var statements` or `function statements` directly modify the global object
                    // so we have to write undefined into it even if there is no initializer
                    // because of enumeration
                    if (isTopLevel) {
                        val hoistingGraphs = buildLiteral(UndefinedValue)(hoistingNodeInfo) ~> new LexicalWriteNode(name)(hoistingNodeInfo) ~> new PopNode()(hoistingNodeInfo)
                        hoistableGraph ~>= hoistingGraphs
                    }
                }

                def buildVariableDeclaration(node: ast.VariableDeclaration, env: LexicalEnv): (Graph, LexicalEnv) = {
                    val ast.VariableDeclaration(decls, kind) = node

                    val bindingNames = decls map { _.id } flatMap { gatherBindingNamesFromPattern }
                    val newEnv = if (kind == ast.VariableDeclarationKind.`var`) {
                        for (name <- bindingNames if !hoistables.contains(name)) {
                            hoistables += name
                            reserveHoistedName(name)
                        }
                        env
                    } else {
                        // kind == let | const
                        addVarsToLexicalEnv(env, bindingNames map { n => (n, makeLocalNameUnique(n)) })
                    }

                    val declGraphs = decls map { buildVarDeclarationAssignment(_, priority, newEnv) }

                    (Graph.concat(declGraphs), newEnv)
                }

                val result: Graph = stmt match {
                    case ast.LabeledStatement(ast.Identifier(name), body) =>
                        if (labels.contains(name)) {
                            throw new BuildException(s"Label '$name' already declared")
                        }


                        val (graph, resultEnv) = buildStatement(body, priority + 1, labels + (name -> here), Some(here), env)
                        newEnv = resultEnv
                        graph.endOption map {
                            end =>
                                if (end eq here.exit) {
                                    graph
                                } else {
                                    graph ~> here.exit
                                }
                        } getOrElse {
                            // if there was nothing done in the statement we also need no after merger!
                            EmptyGraph
                        }
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
                        def debugNode = new DebugNode(ops, stmt.loc.map(_.start.line))
                        innerExprOpt match {
                            case Some(expr) =>
                                val exprGraph = buildExpression(expr, priority, env)
                                exprGraph ~> debugNode ~> new PopNode()
                            case None =>
                                debugNode
                        }

                    case ast.ExpressionStatement(expr) =>
                        val exprGraph = buildExpression(expr, priority, env)
                        popToResult(exprGraph)

                    case ast.TryStatement(tryBody, catchHandler, finallyHandler) =>
                        val finallyBuilder = finallyHandler map {
                            case ast.BlockStatement(finallyBody) =>
                                () => buildInnerBlock(block.inner(), finallyBody, priority + 1, newInnerBlockEnv())
                        }


                        // build catch
                        val tryCatchGraph = catchHandler match {
                            case Some(ast.CatchClause(pattern, catchBody)) =>
                                val catchMerger = new graph.MergeNode(MergeType.CatchMerger)(info.copy(priority = priority + 1))

                                val tryBlock = innerBlock.inner(catchEntry = Some(catchMerger))
                                val tryGraph = buildInnerBlock(tryBlock, tryBody.body, priority + 2, newInnerBlockEnv())

                                val catchEnv = newInnerBlockEnv()
                                val expBindingGraph = buildPatternBinding(pattern, priority + 1, catchEnv, pushWrittenValueToStack = false)
                                val catchBlock = innerBlock.inner(finalizer = finallyBuilder)
                                val catchGraph = buildInnerBlock(catchBlock, catchBody.body, priority + 1, catchEnv)

                                val afterMerger = new graph.MergeNode
                                // we need the jump or we wont find the catch branch with visitors
                                val jmpInfo = info.copy(priority = info.priority + 2)
                                tryGraph ~> new graph.JumpNode(afterMerger)(jmpInfo) ~> catchMerger ~> expBindingGraph ~> catchGraph ~> afterMerger

                            case None =>
                                buildInnerBlock(innerBlock.inner(finalizer = finallyBuilder), tryBody.body, priority + 1, newInnerBlockEnv())
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
                        assert(testGraph != EmptyGraph)
                        Graph(testGraph.begin, merger)

                    case ast.WhileStatement(test, body) =>
                        val loopMergerInfo = info.copy(priority = priority + 1)
                        val loopMerger = new graph.MergeNode(MergeType.Fixpoint)(loopMergerInfo)
                        val loopTarget = here
                        loopTarget.entry = loopMerger
                        val afterMerger = loopTarget.exit
                        val testGraph = buildExpression(test, priority + 2, env)
                        val loopBlock = innerBlock.inner(loopTarget = Some(loopTarget))
                        val bodyGraph = buildInnerBlock(loopBlock, Seq(body), priority + 2, newInnerBlockEnv())
                        val cond = new graph.CondJumpNode(bodyGraph.begin(loopMerger), afterMerger)(info.copy(priority = priority + 2))

                        loopMerger ~> testGraph ~> cond
                        bodyGraph ~> loopMerger
                        Graph(loopMerger, afterMerger)

                    case ast.DoWhileStatement(body, test) =>
                        val loopMergerInfo = info.copy(priority = priority + 1)
                        val loopMerger = new graph.MergeNode(MergeType.Fixpoint)(loopMergerInfo)
                        val beforeTestMerger = new graph.MergeNode()(loopMergerInfo.copy(priority = priority + 2))
                        val loopTarget = here
                        loopTarget.entry = beforeTestMerger
                        val afterMerger = loopTarget.exit
                        val testGraph = buildExpression(test, priority + 2, env)
                        val loopBlock = innerBlock.inner(loopTarget = Some(loopTarget))
                        val bodyGraph = buildInnerBlock(loopBlock, Seq(body), priority + 3, newInnerBlockEnv())
                        val cond = new graph.CondJumpNode(loopMerger, afterMerger)(info.copy(priority = priority + 2))
                        // todo: this might also be solved by a code-cleanup-visitor
                        val beforeTestGraph: Graph = if (beforeTestMerger.hasPred) beforeTestMerger else EmptyGraph

                        loopMerger ~> bodyGraph ~> beforeTestGraph ~> testGraph ~> cond
                        Graph(loopMerger, afterMerger)

                    case ast.ForStatement(init, test, update, body) =>
                        val source = stmt.loc

                        val (initGraph, forEnv) = init map {
                            case decl: ast.VariableDeclaration =>
                                buildVariableDeclaration(decl, newInnerBlockEnv())

                            case expr: ast.Expression =>
                                val initGraph = buildExpression(expr, priority, env) ~> new PopNode
                                (initGraph, env)
                        } getOrElse (EmptyGraph, env)


                        val innerNodeInfo = info.copy(priority + 2, lexicalEnv = forEnv)
                        val loopMergerInfo = info.copy(priority + 1, lexicalEnv = forEnv)
                        val loopMerger = new graph.MergeNode(MergeType.Fixpoint)(loopMergerInfo)


                        val (updateGraph, continueMerger) = update map {
                            updateExpr =>
                                val exprGraph = buildExpression(updateExpr, priority + 2, forEnv)
                                // pop after pushing the update-expr's result to the stack
                                // because the update expression should not influence the result value
                                val continueMerger = new graph.MergeNode()(innerNodeInfo)
                                val updateGraph = continueMerger ~> exprGraph ~> new PopNode()(innerNodeInfo)
                                (updateGraph, continueMerger)
                        } getOrElse (EmptyGraph, loopMerger)

                        val loopTarget = here
                        loopTarget.entry = continueMerger
                        val afterMerger = loopTarget.exit
                        val loopBlock = innerBlock.inner(loopTarget = Some(loopTarget))
                        val bodyGraph = buildInnerBlock(loopBlock, Seq(body), priority + 3, newInnerBlockEnv(forEnv))


                        val testAndJumpGraph: Graph = test match {
                            case Some(testExpr) =>

                                val testGraph = buildExpression(testExpr, priority + 2, forEnv)
                                val cond = new graph.CondJumpNode(bodyGraph.begin(loopMerger), afterMerger)(innerNodeInfo)
                                testGraph ~> cond
                            case None =>
                                new JumpNode(bodyGraph.begin(loopMerger))(innerNodeInfo) ~> afterMerger
                        }

                        initGraph ~> loopMerger ~> testAndJumpGraph
                        bodyGraph ~> updateGraph ~> loopMerger
                        Graph(initGraph.begin(loopMerger), afterMerger)

                    case node: ast.VariableDeclaration =>
                        val (declGraph, envWithDecl) = buildVariableDeclaration(node, env)
                        newEnv = envWithDecl
                        declGraph

                    case decl: ast.FunctionDeclaration =>
                        //lazy val hoistingInfo: Node.Info = info.copy(lexicalEnv = blockHoistingEnv)
                        val name = decl.id.getOrElse(throw new AssertionError("Expected id on function declaration")).name
                        val funcGraph = buildFunction(decl)
                        val hoistingGraph = funcGraph ~> new LexicalWriteNode(name) ~> new PopNode()

                        if (!hoistables.contains(name)) {
                            hoistables += name
                            reserveHoistedName(name)
                        }
                        blockHoistableGraph ~>= hoistingGraph

                        EmptyGraph

                    case _: ast.EmptyStatement =>
                        EmptyGraph

                    case _: ast.DebuggerStatement =>
                        EmptyGraph

                    case _: ast.Directive =>
                        throw new AssertionError("Directives should not appear here")

                    case ast.ReturnStatement(returnExpr) =>
                        assert(returnBlockInfo != null, "Can't return in top level block")

                        val returnExprGraph = returnExpr match {
                            case Some(expr) =>
                                buildExpression(expr, priority, env)

                            case None =>
                                buildLiteral(UndefinedValue)
                        }

                        val returnJumpGraph = buildJump(returnBlockInfo, returnMergeNode, priority, env)

                        returnExprGraph ~> returnJumpGraph ~> new PopNode()
                }

                return (result, newEnv)
            }

            private def buildInnerBlock(block: BlockInfo, statements: Seq[ast.Statement], priority: Int, initialEnv: LexicalEnv): Graph = {
                val builder = new BlockBuilder(strict, block, hoistingEnv, initialEnv, priority)
                builder.build(statements)
            }

            private def buildInnerBlockWithNewLexObj(block: BlockInfo, statements: Seq[ast.Statement], priority: Int, initialEnv: LexicalEnv): Graph = {
                val newDeclEnv = new LexicalEnv(Some(initialEnv), true, LexicalEnv.Behavior.Declarative(Map.empty))
                val builder = new BlockBuilder(strict, block, hoistingEnv, newDeclEnv, priority)
                builder.build(statements)
            }

            def build(statements: Seq[ast.Statement]): Graph = {
                assert(!done)

                // check that directives only appear at the beginning
                assert(statements.span(_.isInstanceOf[ast.Directive])._2.forall(!_.isInstanceOf[ast.Directive]))

                var bodyGraph: Graph = EmptyGraph
                var visitedNonDirective = false
                var env = blockLexicalEnv

                statements foreach {
                    case ast.Directive(_, directive) =>
                        assert(!visitedNonDirective)

                        if (directive == "use strict") {
                            strict = true
                        }

                        // build expression
                        implicit val info: Node.Info = Node.Info(blockPriority, env, functionFrame, block.catchEntry, None)
                        val stringExpr = new graph.PopNode ~> buildLiteral(StringValue(directive))
                        bodyGraph = bodyGraph ~> stringExpr

                    case stmt =>
                        visitedNonDirective = true
                        val (stmtGraph, resultEnv) = buildStatement(stmt, blockPriority, block.labelTargets, None, env)
                        env = resultEnv
                        bodyGraph = bodyGraph ~> stmtGraph
                }
                done = true
                blockHoistableGraph ~> bodyGraph
            }
        }


        def buildTopLevel(program: ast.Program, strict: Boolean): ScriptGraph = {
            assert(functionPriority == 0)
            val priority = 0
            assert(!done)
            val globalEnv = new LexicalEnv(None, true, LexicalEnv.Behavior.Hoisted(hoistables))
            val firstVarsEnv = new LexicalEnv(Some(globalEnv), true, LexicalEnv.Behavior.Declarative(Map.empty))
            implicit val info: Node.Info = Node.Info(priority, globalEnv, functionFrame, None)
            hoistingNodeInfo = info
            val builder = new BlockBuilder(false, new BlockInfo(None, Map.empty, None, None, None), globalEnv, firstVarsEnv, priority)
            val graph = builder.build(program.body collect { case stmt: ast.Statement => stmt })
            val endNode = new EndNode
            val scriptGraph = new PushLexicalFrameNode("main-block", takeFromStack = false) ~> hoistableGraph ~> graph ~> endNode

            done = true
            ScriptGraph(scriptGraph.begin(endNode), endNode)
        }

        def buildFunction(name: Option[String], params: Seq[ast.Pattern], body: ast.ArrowFunctionBody, callSite: CallInstance.RecursionAble, catchTarget: Option[MergeNode], initialEnv: LexicalEnv): Graph = {
            assert(!done)
            val priority = functionPriority
            val hasPatternMatching = params.exists(!_.isInstanceOf[ast.Identifier])

            val initialNodeInfo = Node.Info(priority, initialEnv, functionFrame, catchTarget, None)
            var prologGraph: Graph = EmptyGraph

            val (argEnv, argNodeInfo) = if (!hasPatternMatching) {
                // the parameter-names are bound to the arguments object
                val paramNames = params.map { _.asInstanceOf[ast.Identifier].name }
                val paramNamesMap = paramNames.zipWithIndex.toMap
                prologGraph ~>= new DupNode(1)(initialNodeInfo)
                prologGraph ~>= new PushLexicalFrameNode("args", takeFromStack = true)(initialNodeInfo)
                val argEnv = LexicalEnv(Some(initialEnv), pushesObject = true, LexicalEnv.Behavior.Argument(paramNamesMap))
                (argEnv, initialNodeInfo.copy(lexicalEnv = argEnv))
            } else (initialEnv, initialNodeInfo)


            // create hoisting env and object
            prologGraph ~>= new PushLexicalFrameNode("func", false)(argNodeInfo)
            val funcHoistingEnv = LexicalEnv(Some(argEnv), pushesObject = true, LexicalEnv.Behavior.Hoisted(hoistables))
            hoistingNodeInfo = initialNodeInfo.copy(lexicalEnv = funcHoistingEnv)

            // first write arguments object to hoisting object
            val argumentNames = params.flatMap(gatherBindingNamesFromPattern).toSet
            if (argumentNames.contains("arguments")) {
                prologGraph ~>= new DupNode(1)(hoistingNodeInfo)
                prologGraph ~>= new LexicalWriteNode("arguments")(hoistingNodeInfo)
            }

            if (hasPatternMatching) {
                // the parameter-names are not bound to the arguments object
                // write the arguments into the hoistingGraph
                ???
            }

            prologGraph ~>= new graph.PopNode()(hoistingNodeInfo)

            val normalizedBody = body match {
                case ast.FunctionBody(stmts) =>
                    // this is a normal function definition... nothing to do
                    stmts

                case expr: ast.Expression =>
                    // this is an arrow function which has only one expression which is then returned
                    // e.g.: (arg) => arg + 1
                    // wrap it into a return statement and use that as body
                    Seq(ast.ReturnStatement(Some(expr), expr.loc))
            }
            returnBlockInfo = new BlockInfo(None, Map.empty, None, catchTarget, None)
            returnMergeNode = new MergeNode()(initialNodeInfo)
            val builder = new BlockBuilder(false, returnBlockInfo, funcHoistingEnv, funcHoistingEnv, priority + 1)
            val bodyGraph = builder.build(normalizedBody)

            // build default return
            val epilogGraph = new LiteralNode(UndefinedValue)(initialNodeInfo.copy(priority = priority + 1)) ~> returnMergeNode ~> new RetNode(callSite)(initialNodeInfo)

            // put everything together
            prologGraph ~> hoistableGraph ~> bodyGraph ~> epilogGraph
        }
    }

    def buildTemplate(scriptAst: ast.Program): Templates.Script =  new Templates.Script {
        override def instantiate(): ScriptGraph = {
            val builder = new FunctionBuilder(isTopLevel = true, new Node.CallFrame(None, None), 0)
            val graph = builder.buildTopLevel(scriptAst, strict = false)
            new StackAnnotationVisitor(isFunction = false).start(graph)
            graph
        }
    }

    private case class FunctionTemplate(name: Option[String], params: Seq[ast.Pattern], body: ast.ArrowFunctionBody, lexicalEnv: LexicalEnv) extends CallableInfo {
        override def anchor: AnyRef = body

        override def instantiate(onReturn: CallableInfo.ReturnHandler, priority: Int, catchTarget: Option[MergeNode], callSiteFrame: Node.CallFrame): CallInstance = {
            class InlinedCallInstanceImpl(protected override val onReturn: CallableInfo.ReturnHandler) extends InlinedCallInstance {
                /*override*/ var entryNode: Node = _
            }
            val callInstance = new InlinedCallInstanceImpl(onReturn)
            val functionBuilder = new FunctionBuilder(isTopLevel = false, (anchor -> callInstance) :: callSiteFrame, priority)
            val funcGraph: Graph = functionBuilder.buildFunction(name, params, body, callInstance, catchTarget, lexicalEnv)
            callInstance.entryNode = funcGraph.begin
            new StackAnnotationVisitor(isFunction = true).start(funcGraph)
            callInstance
        }
    }
}