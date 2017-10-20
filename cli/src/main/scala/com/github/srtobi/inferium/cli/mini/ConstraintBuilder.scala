package com.github.srtobi.inferium.cli.mini

import com.github.srtobi.inferium.core.UId

import scala.collection.mutable

private class HeapBuilder(solver: CSolver, begin: HeapNode) {

  private val prevHeaps = mutable.ArrayBuffer.empty[HeapNode]
  prevHeaps += begin

  private def setTo(node: HeapNode): Unit = {
    prevHeaps.clear()
    prevHeaps += node
  }
  private def connectTo(node: HeapNode): Unit = {
    prevHeaps.foreach(prev => prev.flowsTo(node))
    setTo(node)
  }

  def makeEmpty(): Unit = prevHeaps.clear()

  def makeRead(targetObject: TypeUsage, property: String): TypeVariable = {
    val heap = new ReadObjectPropertyHeap(targetObject, property)
    connectTo(heap)
    return heap.resultType
  }

  def makeWrite(targetObject: TypeUsage, property: String, value: TypeVariable): Unit = {
    val heap = new WriteObjectPropertyHeap(targetObject, property, value)
    connectTo(heap)
  }

  def newObject(): TypeVariable = {
    val heap = new NewObjectHeap
    connectTo(heap)
    return heap.newObjectRefType
  }

  def merge(save: Seq[HeapNode]): Unit = {
    prevHeaps ++= save
  }

  def save(): Seq[HeapNode] = {
    val s = prevHeaps.clone()
    prevHeaps.clear()
    return s
  }

  def restore(save: Seq[HeapNode]): Unit = {
    prevHeaps.clear()
    prevHeaps ++= save
  }
}

private class Environment(val context: Option[Environment], val args: Seq[String], val solver: CSolver) {

  private val vars = mutable.HashMap.empty[String, TypeVariable]

  val heapBegin = new HeapNode
  val heapEnd = new HeapNode
  val heap = new HeapBuilder(solver, heapBegin)
  val depth: Int = context.map(_.depth + 1).getOrElse(0)
  val scopeObjects: Seq[TypeVariable] = Seq.fill(depth + 1)(new TypeVariable)
  val scope: TypeVariable = scopeObjects.head
  // write arguments into local variables
  val argTypes: Seq[TypeVariable] = args.map {
    argName =>
      val argType = new TypeVariable
      heap.makeWrite(scope, argName, argType)
      argType
  }
  val returnType = new TypeVariable
  val functionType = new FunctionType(null, returnType, scopeObjects, heapBegin, heapEnd)

  private def lookupContextDepth(name: String): Int = vars.get(name) match {
    case Some(_) => depth
    case None => context.map(_.lookupContextDepth(name)).getOrElse(throw new IllegalArgumentException(s"Unknown variable '$name'"))
  }

  private def lookupContext(name: String): TypeVariable = scopeObjects(lookupContextDepth(name))

  def readLocal(name: String): TypeVariable = {
    val scope = lookupContext(name)
    return heap.makeRead(scope, name)
  }

  def writeLocal(name: String, value: TypeVariable): Unit = {
    if (!vars.contains(name))
      throw new IllegalArgumentException(s"Unknown variable '$name'")
    makeLocal(name, value)
  }

  def makeLocal(name: String, init: TypeVariable): Unit = {
    if (vars.contains(name))
      throw new IllegalArgumentException(s"Variable '$name' already exists")
    val scope = lookupContext(name)
    heap.makeWrite(scope, name, init)
  }
}


class Globals {
  val undefinedType = new ConcreteType("undefined")
  val numberType = new ConcreteType("number")
  val concreteNumberType = mutable.HashMap.empty[String, ConcreteType]
  val concreteStringType = mutable.HashMap.empty[String, ConcreteType]
  val trueType = new ConcreteType("true")
  val falseType = new ConcreteType("false")
}

private class ConstraintBuilder private (val solver: CSolver, prevEnv: Option[Environment], globals: Globals, params: Seq[String]) {
  import Ast._
  import globals._

  val env = new Environment(prevEnv, params, solver)
  val heap: HeapBuilder = env.heap

  private[this] def visitExpression(expression: Expression): TypeVariable = expression match {
    case UndefinedLiteral() =>
      undefinedType
    case NumberLit(num) =>
      concreteNumberType.getOrElseUpdate(num, new ConcreteType(num))
    case BooleanLit(b) =>
      if (b) trueType else falseType
    case Call(_, _) => ???
    case Function(args, body) =>
      val builder = new ConstraintBuilder(solver, Some(env), globals, args)
      builder.buildFunction(body)
      val f = builder.env.functionType
      // wire all scopes + new Scope into function
      assert(env.scopeObjects.length == f.scopes.tail.length)
      env.scopeObjects.zip(f.scopes.tail).foreach {
        case (from, to) => from.flowsTo(to)
      }
      f
    case Identifier(name) =>
      env.readLocal(name)
    case Object(properties) =>
      val obj = heap.newObject()
      properties.foreach {
        case Property(name, init) =>
          val result = visitExpression(init)
          heap.makeWrite(obj, name, result)
      }
      obj
    case Operator("-", left, right) =>
      visitExpression(left).flowsTo(numberType)
      visitExpression(right).flowsTo(numberType)
      numberType
    case Operator(_, _, _) => ???
    case PropertyAccess(base, property) =>
      val target = visitExpression(base)
      heap.makeRead(target, property)
    case StringLiteral(str) =>
      concreteStringType.getOrElseUpdate(str, new ConcreteType(str))
  }

  private[this] def visitStatement(stmt: Statement): Unit = stmt match {
    case ExpressionStmt(expr) =>
      visitExpression(expr)

    case VarStmt(name, init) =>
      val result = visitExpression(init)
      env.makeLocal(name, result)

    case AssignmentStmt(Identifier(name), expr) =>
      val result = visitExpression(expr)
      env.writeLocal(name, result)

    case AssignmentStmt(PropertyAccess(base, property), expr) =>
      val target = visitExpression(base)
      val result = visitExpression(expr)
      heap.makeWrite(target, property, result)

    case AssignmentStmt(_, _) =>
      throw new IllegalArgumentException("Can only assign to variables and properties!")

    case IfStmt(cond, successBlk, fail) =>
      visitExpression(cond)
      val beforeSuccess = heap.save()
      visitBlock(successBlk)
      val afterSuccess = heap.save()
      fail match {
        case Some(failBlk) =>
          heap.restore(beforeSuccess)
          visitBlock(failBlk)
          heap.merge(afterSuccess)
        case None =>
          heap.merge(beforeSuccess)
      }

    case ReturnStmt(expr) =>
      val retValue = expr.map(visitExpression).getOrElse(undefinedType)
      retValue.flowsTo(env.returnType)
      heap.makeEmpty()
  }

  private[this] def visitBlock(block: Ast.Block): Unit = block.foreach(visitStatement)

  def buildScript(script: Script): Unit = {
    visitBlock(script.main)
    finish()
  }

  def buildFunction(block: Ast.Block): Unit = {
    visitBlock(block)
    finish()
  }

  def finish(): Unit = {

  }
}


object ConstraintBuilder {
  def buildSolver(script: Ast.Script): CSolver = {
    val solver = new CSolver
    val globals = new Globals
    val builder = new ConstraintBuilder(solver, None, globals, Seq())
    builder.buildScript(script)
    return solver
  }
}