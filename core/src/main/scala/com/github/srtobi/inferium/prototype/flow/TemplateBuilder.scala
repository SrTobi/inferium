package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast
import com.github.srtobi.inferium.prototype.flow.TemplateBuilder.{Closure, FunctionTemplate}

import scala.collection.mutable


private class TemplateBuilder(body: Seq[Ast.Statement], outerClosureValues: Seq[ValueProvider], parameter: Seq[(String, ValueProvider)], outerClosure: Option[Templates.Closure], solver: Solver, heap: Heap, inHeap: HeapState, outHeap: HeapState) {

    private def returnVariableName = "@return"

    private var done = false
    private val thisClosureSubmitter = heap.newValueSubmitter()
    private val closureValues = outerClosureValues :+ thisClosureSubmitter

    private val closure = new Closure(outerClosure)
    private def functionTemplate = new FunctionTemplate(closure, parameter.map(_._1))

    private[this] def buildExpression(expr: Ast.Expression, in: HeapState): (ValueProvider, HeapState) = expr match {
        case _ => ???
    }

    private[this] def buildStatement(stmt: Ast.Statement, inHeap: HeapState): HeapState = stmt match {
        case Ast.ExpressionStmt(expr) =>
            val (_, outHeap) = buildExpression(expr, inHeap)
            return outHeap

        case Ast.IfStmt(cond, success, fail) =>
            val (condValue, heapState) = buildExpression(cond, inHeap)
            val thenBranch = buildBranch(success)
            val elseBranch = fail.map(buildBranch)
            val outState = heap.newHeapState()
            new Nodes.Conditional(condValue, thenBranch, elseBranch)(heap, inHeap, outState)
            return outState

        case Ast.AssignmentStmt(Ast.PropertyAccess(base, property), expr) =>
            val (baseValue, heapAfterBase) = buildExpression(base, inHeap)
            val (exprValue, heapAfterExpr) = buildExpression(expr, heapAfterBase)
            val outHeap = heap.newHeapState()
            new Nodes.PropertyWrite(baseValue, property, exprValue)(heap, heapAfterExpr, outHeap)
            return outHeap

        case Ast.AssignmentStmt(Ast.Identifier(name), expr) =>
            return buildLocalAssignment(name, expr, inHeap)

        case Ast.VarStmt(name, expr) =>
            closure.makeNewLocalVariable(name)
            return buildLocalAssignment(name, expr, inHeap)

        case Ast.ReturnStmt(expr) =>
            val heapAfterReturnWrite = expr.map(buildLocalAssignment(returnVariableName, _, inHeap)).getOrElse(inHeap)
            heap.addHeapFlow(heapAfterReturnWrite, outHeap)
            return heap.newHeapState()
    }


    private[this] def buildLocalAssignment(name: String, expr: Ast.Expression, in: HeapState): HeapState = {
        val (exprValue, heapAfterExpr) = buildExpression(expr, in)
        return buildLocalAssignment(name, exprValue, heapAfterExpr)
    }

    private[this] def buildLocalAssignment(name: String, value: ValueProvider, in: HeapState): HeapState = {
        val outHeap = heap.newHeapState()
        val localValue = closureValues(closure.closureIndexForVar(name))
        new Nodes.PropertyWrite(localValue, name, value)(heap, in, outHeap)
        return outHeap
    }

    private[this] def buildBranch(stmts: Seq[Ast.Statement]): (HeapState, HeapState) = {
        val in = heap.newHeapState()
        val out = buildStatements(stmts, in)
        return (in, out)
    }

    private[this] def buildStatements(stmts: Seq[Ast.Statement], initial: HeapState): HeapState = {
        return stmts.foldLeft(initial) {
            case (in, stmt) => buildStatement(stmt, in)
        }
    }

    def build(): Nodes.Function = {
        if (done) {
            throw new IllegalStateException("A builder can only be used once!")
        }

        // allocate return variable
        closure.makeNewLocalVariable(returnVariableName)
        var tmpHeap = buildLocalAssignment(returnVariableName, Ast.UndefinedLiteral(), inHeap)

        // write parameters
        tmpHeap = parameter.foldLeft(tmpHeap) {
            case (heapState, (paramName, paramValue)) =>
                closure.makeNewLocalVariable(paramName)
                buildLocalAssignment(paramName, paramValue, heapState)
        }

        tmpHeap = buildStatements(body, tmpHeap)
        heap.addHeapFlow(tmpHeap, outHeap)
        val readnode = new Nodes.PropertyRead(closureValues.last, returnVariableName)

        done = true
        return new Nodes.Function(readnode.result, inHeap, outHeap)
    }
}


object TemplateBuilder {

    private class FunctionTemplate(override val closure: Templates.Closure, override val parameter: Seq[String]) extends Templates.Function{

        override def instantiate(closures: Seq[ValueProvider], inHeap: HeapState, outHeap: HeapState): Nodes.Function = {
            ???
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
        override def instantiate(solver: Solver, heap: Heap, inHeap: HeapState, outHeap: HeapState): Nodes.Function = {
            val builder = new TemplateBuilder(script.main, Seq(), Seq(), None, solver, heap, inHeap, outHeap)
            return builder.build()
        }
    }
}
