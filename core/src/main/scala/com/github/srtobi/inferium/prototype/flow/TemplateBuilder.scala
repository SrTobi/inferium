package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast
import com.github.srtobi.inferium.prototype.flow.Nodes.BeginNode
import com.github.srtobi.inferium.prototype.flow.TemplateBuilder.{Closure, FunctionTemplate}

import scala.collection.mutable



private class TemplateBuilder(body: Seq[Ast.Statement],
                              outerClosureValues: Seq[Value],
                              parameter: Seq[(String, ValueSourceProvider)],
                              outerClosure: Option[Templates.Closure],
                              endNode: Nodes.Node)(implicit val flowAnalysis: FlowAnalysis) {

    private class NodeBuilder(begin: Nodes.BeginNode) {
        private var cur: Nodes.Node = begin

        def apply[T <: Nodes.Node](node: T): T = {
            assert(cur != null)
            assert(node.next == null || (node eq endNode))
            cur.next = node
            cur = node
            return node
        }

        def end(): Nodes.Node = {
            val res = cur
            cur = null
            return res
        }

        def startOver(node: Nodes.BeginNode): Unit = {
            cur = node
        }
    }

    private def solver = flowAnalysis.solver
    private var done = false
    private val thisClosureValue = solver.newEmptyObject()
    private val closureValues = outerClosureValues :+ thisClosureValue
    private val returns = mutable.Buffer.empty[ValueSourceProvider]

    private val closure = new Closure(outerClosure)


    private def wrapValue(value: Value): ValueSourceProvider = {
        val sink = new ValueSink
        sink.set(value)
        return sink
    }

    private[this] def buildExpression(expr: Ast.Expression, newnode: NodeBuilder): ValueSourceProvider = expr match {
        case Ast.UndefinedLiteral() =>
            val node = newnode(new Nodes.Literal(UndefinedValue))
            return node.result

        case Ast.BooleanLit(bool) =>
            val node = newnode(new Nodes.Literal(solver.boolean(bool)))
            return node.result

        case Ast.NumberLit(n) =>
            val node = newnode(new Nodes.Literal(solver.number(n)))
            return node.result

        case Ast.StringLiteral(str) =>
            val node = newnode(new Nodes.Literal(solver.string(str)))
            return node.result

        case Ast.Identifier(id) =>
            val value = buildLocalRead(id, newnode)
            return value

        case Ast.Operator("-", left, right) =>
            val leftValue = buildExpression(left, newnode)
            val rightValue  = buildExpression(right, newnode)
            val node = newnode(new Nodes.Subtraction(leftValue.newSource(), rightValue.newSource()))
            return node.result

        case Ast.Operator(op, _, _) =>
            throw new IllegalArgumentException(s"Operation $op is currently not permitted!")

        case Ast.Object(props) =>
            val objNode = newnode(new Nodes.NewObject())
            val objValue = objNode.result
            for (prop <- props) {
                val initValue = buildExpression(prop.init, newnode)
                newnode(new Nodes.PropertyWrite(objValue.newSource(), prop.name, initValue.newSource()))
            }
            return objValue

        case Ast.PropertyAccess(base, property) =>
            val baseValue = buildExpression(base, newnode)
            val node = newnode(new Nodes.PropertyRead(baseValue.newSource(), property))
            return node.result

        case Ast.Call(func, args) =>
            val funcValue = buildExpression(func, newnode)
            val argValues = args.map(buildExpression(_, newnode))

            val node = newnode(new Nodes.FunctionCall(funcValue.newSource(), argValues))
            return node.result

        case func: Ast.Function =>
            val templ = new FunctionTemplate(func, closure)
            val node = newnode(new Nodes.Literal(new FunctionValue(templ, closureValues)))
            return node.result
    }

    private[this] def buildStatement(stmt: Ast.Statement, newnode: NodeBuilder): Unit = stmt match {
        case Ast.ExpressionStmt(expr) =>
            buildExpression(expr, newnode)

        case Ast.IfStmt(cond, success, fail) =>
            val condValue = buildExpression(cond, newnode)
            val thenBranch = buildBranch(success)
            val elseBranch = fail.map(buildBranch)
            newnode(new Nodes.Conditional(condValue.newSource(), thenBranch, elseBranch))

        case Ast.AssignmentStmt(Ast.PropertyAccess(base, property), expr) =>
            val baseValue = buildExpression(base, newnode)
            val exprValue = buildExpression(expr, newnode)
            newnode(new Nodes.PropertyWrite(baseValue.newSource(), property, exprValue.newSource()))

        case Ast.AssignmentStmt(Ast.Identifier(name), expr) =>
            buildLocalAssignment(name, expr, newnode)

        case Ast.AssignmentStmt(ast, _) =>
            throw new IllegalArgumentException(s"$ast can not be assigned a value")

        case Ast.VarStmt(name, expr) =>
            buildLocalAssignment(name, expr, newnode)

        case Ast.ReturnStmt(expr) =>
            val returnValue = expr.map(buildExpression(_, newnode))
                .getOrElse(buildExpression(Ast.UndefinedLiteral(), newnode))
            val deadNode = new Nodes.BeginNode()
            val node = newnode(new Nodes.ReturnNode(returnValue.newSource(), Some(deadNode)))
            returns += node.result
            newnode(endNode)
            newnode.startOver(deadNode)
    }


    private[this] def buildLocalAssignment(name: String, expr: Ast.Expression, newNode: NodeBuilder): Unit = {
        val exprValue = buildExpression(expr, newNode)
        buildLocalAssignment(name, exprValue, newNode)
    }

    private[this] def buildLocalAssignment(name: String, value: ValueSourceProvider, newnode: NodeBuilder): Unit = {
        val localValue = closureValues(closure.closureIndexForVar(name))
        newnode(new Nodes.PropertyWrite(wrapValue(localValue).newSource(), name, value.newSource()))
    }

    private[this] def buildLocalRead(name: String, newnode: NodeBuilder): ValueSourceProvider = {
        val closureIdx = closure.closureIndexForVar(name)
        val node = newnode(new Nodes.PropertyRead(wrapValue(closureValues(closureIdx)).newSource(), name))
        return node.result
    }

    private[this] def buildBranch(stmts: Seq[Ast.Statement]): (Nodes.Node, Nodes.Node) = {
        val beginNode = new BeginNode()
        val newNode = new NodeBuilder(beginNode)
        buildStatements(stmts, newNode)
        return (beginNode, newNode.end())
    }

    private[this] def buildStatements(stmts: Seq[Ast.Statement], nodeBuilder: NodeBuilder): Unit = {
        stmts.foreach(buildStatement(_, nodeBuilder))
    }

    private[this] def hoistVars(stmts: Seq[Ast.Statement]): Unit = {
        stmts.foreach {
            case Ast.ExpressionStmt(_) =>
            case Ast.IfStmt(_, success, fail) =>
                hoistVars(success)
                fail.foreach(hoistVars)
            case Ast.AssignmentStmt(_, _) =>
            case Ast.VarStmt(name, _) =>
                closure.makeNewLocalVariable(name)
            case Ast.ReturnStmt(expr) =>
        }
    }

    def build(): (Nodes.Node, Seq[ValueSourceProvider]) = {
        if (done) {
            throw new IllegalStateException("A builder can only be used once!")
        }

        val beginNode = new BeginNode()
        val newNode = new NodeBuilder(beginNode)

        // write parameters
        for((paramName, paramValue) <- parameter) {
            closure.makeNewLocalVariable(paramName)
            buildLocalAssignment(paramName, paramValue, newNode)
        }
        hoistVars(body)

        buildStatements(body, newNode)
        val undefReturnValue = wrapValue(UndefinedValue)
        val node = newNode(new Nodes.ReturnNode(undefReturnValue.newSource(), None))
        returns += node.result
        newNode(endNode)
        newNode.end()
        done = true
        return (beginNode, returns)
    }
}


object TemplateBuilder {

    private class FunctionTemplate(val ast: Ast.Function, override val closure: Templates.Closure)(implicit val flowAnalysis: FlowAnalysis) extends Templates.Function {

        override def parameters: Seq[String] = ast.params

        override def instantiate(closures: Seq[Value], arguments: Seq[ValueSourceProvider], endNode: Nodes.Node): (Nodes.Node, Seq[ValueSourceProvider]) = {
            val builder = new TemplateBuilder(ast.block, closures, parameters.zip(arguments), Some(closure), endNode)
            return builder.build()
        }

    }

    private class Closure(override val outer: Option[Templates.Closure]) extends Templates.Closure {
        val locals = mutable.Set.empty[String]

        override val closureIndex: Int = outer.map(_.closureIndex + 1).getOrElse(0)
        override def hasVar(name: String): Boolean = locals.contains(name)
        override def closureIndexForVar(name: String): Int =
            if (hasVar(name)) closureIndex else outer.map(_.closureIndexForVar(name)).getOrElse(0)

        def makeNewLocalVariable(name: String): Unit =
            if (hasVar(name)) throw new IllegalArgumentException(s"$name was already defined!") else locals += name
    }

    def buildScriptTemplate(script: Ast.Script): Templates.Script = new Templates.Script {
        override def instantiate(flowAnalysis: FlowAnalysis, global: ObjectValue, endNode: Nodes.Node): (Nodes.Node, Seq[ValueSourceProvider]) = {
            val globalClosure = new Closure(None)
            val builder = new TemplateBuilder(script.main, Seq(global), Seq(), Some(globalClosure), endNode)(flowAnalysis)
            return builder.build()
        }
    }
}
