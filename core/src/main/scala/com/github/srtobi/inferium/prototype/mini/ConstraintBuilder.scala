package com.github.srtobi.inferium.prototype.mini

import com.github.srtobi.inferium.prototype.{Ast, LangParser, LangPrinter}
import com.github.srtobi.inferium.core.UId
import fastparse.core.Parsed

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class TypeMapper(private val exprMap: scala.collection.Map[UId, TypeVariable]) {
  def lookup(node: Ast.Node): Option[TypeVariable] = exprMap.get(node.id)
}


class HeapBuilder(solver: CSolver, begin: HeapNode) {


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

  def makeRead(targetObject: TypeVariable, property: String): TypeVariable = {
    val heap = new ReadObjectPropertyHeap(solver, targetObject, property)
    connectTo(heap)
    return heap.resultType
  }

  def makeWrite(targetObject: TypeVariable, property: String, value: TypeVariable): Unit = {
    val heap = new WriteObjectPropertyHeap(solver, targetObject, property, value)
    connectTo(heap)
  }

  def makeCall(function: TypeVariable, args: Seq[TypeVariable]): TypeVariable = {
    val nextHeap = new HeapNode(solver)
    val call = new CallSite(solver, function, args, nextHeap)
    connectTo(call)
    setTo(nextHeap)
    return call.returnType
  }

  def newObject(): TypeVariable = {
    val heap = new NewObjectHeap(solver)
    connectTo(heap)
    return heap.newObjectRefType
  }

  def merge(save: Seq[HeapNode]): Unit = {
    prevHeaps ++= save
  }

  def save(): Seq[HeapNode] = {
    val s = prevHeaps.clone()
    return s
  }

  def restore(save: Seq[HeapNode]): Unit = {
    prevHeaps.clear()
    prevHeaps ++= save
  }

  def finish(end: HeapNode): Unit = {
    connectTo(end)
  }
}

class Environment(val surrounding: Option[Environment], val args: Seq[String], val solver: CSolver) {

  val vars = mutable.HashMap.empty[String, TypeVariable]
  val depth: Int = surrounding.map(_.depth + 1).getOrElse(0)

  def lookupContextDepth(name: String): (Int, TypeVariable) = if (vars.contains(name)) {
    (depth, vars(name))
  } else {
    surrounding.map(_.lookupContextDepth(name)).getOrElse(throw new IllegalArgumentException(s"Unknown variable '$name'"))
  }
}

class EnvAdapter(val env: Environment, funcdef: Ast.Function) {
  import env._
  val heapBegin = new HeapNode(solver)
  val heapEnd = new HeapNode(solver)
  val heap = new HeapBuilder(solver, heapBegin)
  val scopeObjects: Seq[TypeVariable] = Seq.fill(depth + 1)(new TypeVariable(solver))
  val scope: TypeVariable = scopeObjects.last
  heap.newObject().flowsTo(scope)
  // write arguments into local variables
  val argTypes: Seq[TypeVariable] = args.map {
    argName =>
      val argType = new TypeVariable(solver).marked()
      vars.update(argName, argType)
      heap.makeWrite(scope, argName, argType)
      argType
  }
  val returnType = new TypeVariable(solver)
  val functionType = new FunctionType(solver, argTypes, returnType, scopeObjects.init, funcdef, env.surrounding)

  private def lookupContext(name: String): (TypeVariable, TypeVariable) = lookupContextDepth(name) match {
    case (d, v) => (scopeObjects(d), v)
  }

  def readLocal(name: String): TypeVariable = {
    val (scope, v) = lookupContext(name)
    val result = heap.makeRead(scope, name)
    v.flowsTo(result)
    return result
  }

  def writeLocal(name: String, value: TypeVariable): Unit = {
    val (scope, _) = lookupContext(name)
    heap.makeWrite(scope, name, value)
  }

  def makeLocal(name: String, init: TypeVariable): Unit = {
    if (vars.contains(name))
      throw new IllegalArgumentException(s"Variable '$name' already exists")
    vars.update(name, new TypeVariable(solver))
    writeLocal(name, init)
  }
}


class Globals(solver: CSolver) {
  val globalContext = new NodeContext(solver, None)
  solver.pushContext(globalContext)
  val undefinedType = new ConcreteType(solver, "undefined")
  val numberType = new ConcreteType(solver, "number")
  val concreteNumberType = mutable.HashMap.empty[String, ConcreteType]
  val concreteStringType = mutable.HashMap.empty[String, ConcreteType]
  val trueType = new ConcreteType(solver, "true")
  val falseType = new ConcreteType(solver, "false")
}

class ConstraintBuilder(val solver: CSolver, funcdef: Ast.Function, prevEnv: Option[Environment], prevContext: NodeContext, globals: Globals, val astMap: mutable.HashMap[UId, TypeVariable]) {
  import Ast._
  import globals._

  val params = funcdef.params
  val context = new NodeContext(solver, Some(prevContext))
  solver.pushContext(context)
  val env = new EnvAdapter(new Environment(prevEnv, params, solver), funcdef)
  val heap: HeapBuilder = env.heap
  val heapEnds: ArrayBuffer[HeapNode] = mutable.ArrayBuffer.empty[HeapNode]
  solver.registerHeapStart(env.heapBegin)

  private[this] def regType(ast: Ast.Node, ty: TypeVariable): Unit = astMap.update(ast.id, ty)
  private[this] def visitExpression(expression: Expression): TypeVariable = expression match {
    case UndefinedLiteral() =>
      undefinedType
    case NumberLit(num) =>
      concreteNumberType.getOrElseUpdate(num, new ConcreteType(solver, num))
    case BooleanLit(b) =>
      if (b) trueType else falseType
    case Call(func, args) =>
      heap.makeCall(visitExpression(func), args.map(visitExpression))
    case func@Function(args, body) =>
      val builder = new ConstraintBuilder(solver, func, Some(env.env), context, globals, astMap)
      builder.build()
      val ft = builder.env.functionType
      // wire all scopes + new Scope into function
      assert(env.scopeObjects.length == ft.scopes.length)
      env.scopeObjects.zip(ft.scopes).foreach {
        case (from, to) => from.flowsTo(to)
      }
      regType(expression, env.functionType)
      ft
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
      concreteStringType.getOrElseUpdate(str, new ConcreteType(solver, str))
  }

  private[this] def visitStatement(stmt: Statement): Unit = stmt match {
    case ExpressionStmt(expr) =>
      val ty = visitExpression(expr)
      regType(stmt, ty)

    case VarStmt(name, init) =>
      val result = visitExpression(init)
      env.makeLocal(name, result)
      regType(stmt, result)

    case AssignmentStmt(Identifier(name), expr) =>
      val result = visitExpression(expr)
      env.writeLocal(name, result)
      regType(stmt, result)

    case AssignmentStmt(PropertyAccess(base, property), expr) =>
      val target = visitExpression(base)
      val result = visitExpression(expr)
      heap.makeWrite(target, property, result)
      regType(stmt, result)

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
      heapEnds ++= heap.save()
      heap.makeEmpty()
      regType(stmt, retValue)
  }

  private[this] def visitBlock(block: Ast.Block): Unit = block.foreach(visitStatement)

  def build(): Unit = {
    visitBlock(funcdef.block)
    finish()
  }

  def finish(): Unit = {
    heap.merge(heapEnds)
    env.env.vars.foreach {
      case (name, v) => heap.makeRead(env.scope, name).flowsTo(v)
    }
    heap.finish(env.heapEnd)
    solver.popContext()
  }
}


object ConstraintBuilder {
  def buildSolver(script: Ast.Script): (CSolver, TypeMapper) = {
    val solver = new CSolver
    val globals = new Globals(solver)
    val map = mutable.HashMap.empty[UId, TypeVariable]
    val builder = new ConstraintBuilder(solver, Ast.Function(Seq(), script.main), None, globals.globalContext, globals, map)
    builder.build()
    solver.popContext()
    solver.solve(globals)
    return (solver, new TypeMapper(map))
  }
}

object ConstraintBuilderTest {
  def show(base: TypeVariable, ty: TypeUsage): Boolean = ty.mark
  def printClosed(ty: TypeUsage): String = ty match {
    case f: FunctionType => s"(${f.args.map(printTypeVar).mkString(", ")}) -> ${printTypeVar(f.ret)}"
    case _ => ty.toString
  }
  def printTypeVar(ty: TypeVariable): String = ty match {
    case _ if ty.fencing => printClosed(ty)
    case _ => s"[${ty.in.filter(show(ty, _)).map(printClosed).mkString(" | ")} ≤ $ty ≤ ${ty.out.filter(show(ty, _)).map(printClosed).mkString(" & ")}]"
  }

  def test(script: Ast.Script): Unit = {
    println("---------------------")
    val (solver, ty) = ConstraintBuilder.buildSolver(script)
    script.main.foreach {
      stmt =>
        println(LangPrinter.print(stmt) + "  // " + ty.lookup(stmt).map(printTypeVar).getOrElse("()"))
    }
    println("---------------------")
    //solver.printTypes()
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |var slot = $(x) {
        |  return $() {
        |    return x
        |  }
        |};
        |var one = slot(1);
        |var two = slot(2);
        |var y = two();
        |var x = one();
        |var f = $(f) {
        | f(3)
        |}
      """.stripMargin
    LangParser.script.parse(code) match {
      case Parsed.Success(ast, _) =>
        //println(ast)
        println(LangPrinter.print(ast))
        test(ast)
      case f@Parsed.Failure(lastParser, _, extra) =>
        println(f)
    }
  }
}