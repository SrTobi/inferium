package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast
import com.github.srtobi.inferium.prototype.flow.TemplateBuilder.{Closure, FunctionTemplate}

import scala.collection.mutable


private class TemplateBuilder(body: Seq[Ast.Statement],
                              outerClosureValues: Seq[Value],
                              parameter: Seq[(String, ValueHandle)],
                              outerClosure: Option[Templates.Closure],
                              solver: Solver,
                              heap: Heap,
                              endNode: Nodes.Node,
                              returnMerger: ValueHandleMerger) {

    private class NodeBuilder {
        var begin: Nodes.BeginNode = new Nodes.BeginNode
        private var cur: Nodes.Node = begin

        def apply[T <: Nodes.Node](node: T): T = {
            assert(cur != null)
            assert(node.next == null)
            cur.next = node
            cur = node
            return node
        }

        def end(): Nodes.Node = {
            val res = cur
            cur = null
            return res
        }

        def startOver(): Unit = {
            begin = new Nodes.BeginNode
            cur = begin
        }
    }

    private var done = false
    private val thisClosureValue = solver.newEmptyObject()
    private val closureValues = outerClosureValues :+ thisClosureValue

    private val closure = new Closure(outerClosure)

    private[this] def buildExpression(expr: Ast.Expression, newnode: NodeBuilder): ValueHandle = expr match {
        case Ast.UndefinedLiteral() =>
            val node = newnode(new Nodes.Literal(solver.undefined())(heap))
            return node.result

        case Ast.BooleanLit(bool) =>
            val node = newnode(new Nodes.Literal(solver.boolean(bool))(heap))
            return node.result

        case Ast.NumberLit(n) =>
            val node = newnode(new Nodes.Literal(solver.number(n))(heap))
            return node.result

        case Ast.StringLiteral(str) =>
            val node = newnode(new Nodes.Literal(solver.string(str))(heap))
            return node.result

        case Ast.Identifier(id) =>
            val value = buildLocalRead(id, newnode)
            return value

        case Ast.Operator("-", left, right) =>
            val leftValue = buildExpression(left, newnode)
            val rightValue  = buildExpression(right, newnode)
            val node = newnode(new Nodes.Subtraction(leftValue, rightValue)(heap, solver))
            return node.result

        case Ast.Operator(op, _, _) =>
            throw new IllegalArgumentException(s"Operation $op is currently not permitted!")

        case Ast.Object(props) =>
            val objNode = newnode(new Nodes.NewObject()(heap, solver))
            val objValue = objNode.result
            for (prop <- props) {
                val initValue = buildExpression(prop.init, newnode)
                newnode(new Nodes.PropertyWrite(objValue, prop.name, initValue))
            }
            return objValue

        case Ast.PropertyAccess(base, property) =>
            val baseValue = buildExpression(base, newnode)
            val node = newnode(new Nodes.PropertyRead(baseValue, property)(heap))
            return node.result

        case Ast.Call(func, args) =>
            val funcValue = buildExpression(func, newnode)
            val argValues = args.map(buildExpression(_, newnode))

            val node = newnode(new Nodes.FunctionCall(funcValue, argValues)(heap))
            return node.result

        case func: Ast.Function =>
            val templ = new FunctionTemplate(func, new Closure(Some(closure)))(solver, heap)
            val node = newnode(new Nodes.Literal(FunctionValue(templ, closureValues))(heap))
            return node.result
    }

    private[this] def buildStatement(stmt: Ast.Statement, newnode: NodeBuilder): Unit = stmt match {
        case Ast.ExpressionStmt(expr) =>
            buildExpression(expr, newnode)

        case Ast.IfStmt(cond, success, fail) =>
            val condValue = buildExpression(cond, newnode)
            val thenBranch = buildBranch(success)
            val elseBranch = fail.map(buildBranch)
            newnode(new Nodes.Conditional(condValue, thenBranch, elseBranch)(heap))

        case Ast.AssignmentStmt(Ast.PropertyAccess(base, property), expr) =>
            val baseValue = buildExpression(base, newnode)
            val exprValue = buildExpression(expr, newnode)
            newnode(new Nodes.PropertyWrite(baseValue, property, exprValue))

        case Ast.AssignmentStmt(Ast.Identifier(name), expr) =>
            buildLocalAssignment(name, expr, newnode)

        case Ast.AssignmentStmt(ast, _) =>
            throw new IllegalArgumentException(s"$ast can not be assigned a value")

        case Ast.VarStmt(name, expr) =>
            closure.makeNewLocalVariable(name)
            buildLocalAssignment(name, expr, newnode)

        case Ast.ReturnStmt(expr) =>
            expr.map(buildExpression(_, newnode)).foreach(returnMerger.add(_))
            newnode(endNode)
            newnode.startOver()
    }


    private[this] def buildLocalAssignment(name: String, expr: Ast.Expression, newNode: NodeBuilder): Unit = {
        val exprValue = buildExpression(expr, newNode)
        buildLocalAssignment(name, exprValue, newNode)
    }

    private[this] def buildLocalAssignment(name: String, value: ValueHandle, newnode: NodeBuilder): Unit = {
        val localValue = closureValues(closure.closureIndexForVar(name))
        newnode(new Nodes.LocalWrite(localValue, name, value))
    }

    private[this] def buildLocalRead(name: String, newnode: NodeBuilder): ValueHandle = {
        val closureIdx = closure.closureIndexForVar(name)
        val node = newnode(new Nodes.LocalRead(closureValues(closureIdx), name)(heap))
        return node.result
    }

    private[this] def buildBranch(stmts: Seq[Ast.Statement]): (Nodes.Node, Nodes.Node) = {
        val newNode = new NodeBuilder
        buildStatements(stmts, newNode)
        return (newNode.begin, newNode.end())
    }

    private[this] def buildStatements(stmts: Seq[Ast.Statement], nodeBuilder: NodeBuilder): Unit = {
        stmts.foreach(buildStatement(_, nodeBuilder))
    }

    def build(): Nodes.Node = {
        if (done) {
            throw new IllegalStateException("A builder can only be used once!")
        }

        val newNode = new NodeBuilder

        // write parameters
        for((paramName, paramValue) <- parameter) {
            closure.makeNewLocalVariable(paramName)
            buildLocalAssignment(paramName, paramValue, newNode)
        }

        buildStatements(body, newNode)

        newNode(endNode)
        newNode.end()
        done = true
        return newNode.begin
    }
}


object TemplateBuilder {

    private class FunctionTemplate(val ast: Ast.Function, override val closure: Templates.Closure)(solver: Solver, heap: Heap) extends Templates.Function {

        override def parameters: Seq[String] = ast.params

        override def instantiate(closures: Seq[Value], arguments: Seq[ValueHandle], endNode: Nodes.Node, returnMerger: ValueHandleMerger): Nodes.Node = {
            val builder = new TemplateBuilder(ast.block, closures, parameters.zip(arguments), Some(closure), solver, heap, endNode, returnMerger)
            return builder.build()
        }

    }

    private class Closure(override val outer: Option[Templates.Closure]) extends Templates.Closure {
        val locals = mutable.Set.empty[String]

        override val closureIndex: Int = outer.map(_.closureIndex + 1).getOrElse(0)
        override def hasVar(name: String): Boolean = locals.contains(name)
        override def closureIndexForVar(name: String): Int =
            if (hasVar(name)) closureIndex else outer.map(_.closureIndexForVar(name)).getOrElse(throw new IllegalArgumentException(s"$name is no local variable!"))

        def makeNewLocalVariable(name: String): Unit =
            if (hasVar(name)) throw new IllegalArgumentException(s"$name was already defined!") else locals += name
    }

    def buildScriptTemplate(script: Ast.Script): Templates.Script = new Templates.Script {
        override def instantiate(solver: Solver, heap: Heap, endNode: Nodes.Node): (Nodes.Node, ValueHandleMerger) = {
            val returnMerger = heap.newValueHandleMerger()
            val builder = new TemplateBuilder(script.main, Seq(), Seq(), None, solver, heap, endNode, returnMerger)
            return (builder.build(), returnMerger)
        }
    }
}
