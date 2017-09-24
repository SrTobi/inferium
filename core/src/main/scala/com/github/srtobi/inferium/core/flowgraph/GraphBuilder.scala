package com.github.srtobi.inferium.core.flowgraph

import com.github.srtobi.inferium.core.UId

import scala.annotation.tailrec
import scala.collection.mutable


trait Builder [T] {
  def finish(): T
}

class FinishGuard() {
  private var finished = false
  def isFinished: Boolean = finished
  def guarded[R](f: => R): R = { assert(); f }
  def assert(): Unit = if (finished) throw new IllegalStateException("Tried to alter finished builder")
  def finish(): Unit = { finished = true }
}

private trait NodeBuilding {
  this: Node =>

  var _comments: Seq[String] = Seq.empty
  override def comments: Seq[String] = _comments

  var _inNodes: mutable.Buffer[Node] = mutable.Buffer.empty[Node]
  override def inNodes: Seq[Node] = _inNodes
}

private trait LinearNodeBuilding extends NodeBuilding{
  this: LinearNode =>

  var _next: Node = _
  override def next: Node = _next
}

private trait Edge {
  def setNext(node: Node): Unit
  def node: Option[Node with NodeBuilding]
}

class BlockBuilder(private val fBuilder: FunctionBuilder, private val exceptTarget: Node, private val surrounding: Option[BlockBuilder], prevEdges: Seq[Edge], notifyFinish: (Seq[Edge]) => Unit) {
  private val g = new FinishGuard
  private val vars = mutable.HashMap.empty[String, Int]
  private val function = fBuilder.function
  private var lastEdges = mutable.Buffer.empty[Edge]
  private var numUnfinishedBlocks = 0

  lastEdges ++= prevEdges

  def newVar(name: String): Unit = {
    g.assert()
    if (vars.contains(name))
      throw new IllegalArgumentException(s"$name is already defined")
    vars += (name -> fBuilder.newVar(name))
  }

  def findVar(name: String): LocalVarRef = g.guarded(findVarImpl(name, 0))
  private def findVarImpl(name: String, idx: Int): LocalVarRef = vars
    .get(name)
    .map(ResolvedLocalVarRef(idx, _)(function))
    .getOrElse(surrounding.map(_.findVarImpl(name, idx + 1)).getOrElse(UnresolvedLocalVarRef(name)))

  private def setCurNode[N <: Node with LinearNodeBuilding](openNode: N): N = {
    g.assert()
    if (numUnfinishedBlocks > 0)
      throw new IllegalStateException("Can not continue building block while other blocks are unfinished")

    val edge = new Edge {
      override val node: Option[Node with NodeBuilding] = Some(openNode)
      override def setNext(node: Node): Unit = { openNode._next = node }
    }
    wireNodesToOpenEdges(openNode)
    lastEdges += edge
    return openNode
  }

  private def wireNodesToOpenEdges(node: Node with NodeBuilding): Unit = {
    node._inNodes ++= lastEdges.flatMap(_.node)
    lastEdges.foreach(_.setNext(node))
    lastEdges.clear()
  }

  def toRegisterRef(nodeArgument: NodeArgument): RegisterRef = g.guarded(nodeArgument match {
    case ref: RegisterRef => ref
    case UnresolvedLocalVarRef(name) => ???
    case ref: ResolvedLocalVarRef => RegisterRef(newRegisterLoad(ref).result)
    case value: ValueArgument => RegisterRef(newRegisterLoad(value).result)
  })

  def newReadProperty(base: NodeArgument, property: NodeArgument): RegisterRef = RegisterRef(setCurNode(new ReadPropertyNode(base, property) with LinearNodeBuilding {
    override val result: Register = fBuilder.newRegister(this)
    override val scope: Function = function
    override val exceptNode: Node = exceptTarget
    override val scopeIndex: Int = fBuilder.nextIdx()
  }).result)

  def newWriteProperty(base: NodeArgument, property: NodeArgument, src: NodeArgument): RegisterRef = {
    val regRef = toRegisterRef(src)
    setCurNode(new WritePropertyNode(base, property, regRef) with LinearNodeBuilding {
      override val scope: Function = function
      override val exceptNode: Node = exceptTarget
      override val scopeIndex: Int = fBuilder.nextIdx()
    })
    return regRef
  }

  def newBlock(): BlockBuilder = {
    g.assert()
    if (numUnfinishedBlocks > 0)
      throw new IllegalStateException("Can not continue building block while other blocks are unfinished")
    numUnfinishedBlocks += 1
    return new BlockBuilder(fBuilder, exceptTarget, Some(this), lastEdges.clone(), (edges: Seq[Edge]) => {
      lastEdges.clear()
      lastEdges ++= edges
      numUnfinishedBlocks -= 1
    })
  }

  def newConditional(condition: NodeArgument): (BlockBuilder, BlockBuilder) = {
    g.assert()
    if (numUnfinishedBlocks > 0)
      throw new IllegalStateException("Can not continue building block while other blocks are unfinished")
    numUnfinishedBlocks += 2

    val conditional = new ConditionalBranchNode(condition) with NodeBuilding {
      var _fail: Node = _
      override def fail: Node = _fail

      var _success: Node = _
      override def success: Node = _success

      override val scope: Function = function
      override val exceptNode: Node = exceptTarget
      override val scopeIndex: Int = fBuilder.nextIdx()
    }

    // wire all open nodes with the new conditional
    wireNodesToOpenEdges(conditional)

    val successEdge = new Edge {
      private val _cond = conditional
      override def node: Option[Node with NodeBuilding] = Some(_cond)
      override def setNext(node: Node): Unit = { _cond._success = node }
    }

    val failEdge = new Edge {
      private val _cond = conditional
      override def node: Option[Node with NodeBuilding] = Some(_cond)
      override def setNext(node: Node): Unit = { _cond._fail = node }
    }

    def finishBlock = (edges: Seq[Edge]) => {
      lastEdges ++= edges
      numUnfinishedBlocks -= 1
    }

    return (
      new BlockBuilder(fBuilder, exceptTarget, Some(this), Seq(successEdge), finishBlock),
      new BlockBuilder(fBuilder, exceptTarget, Some(this), Seq(failEdge), finishBlock)
    )
  }

  def value(str: String): StringArgument = StringArgument(str)
  def value(num: Long): IntegerArgument = IntegerArgument(num)
  def value(num: Double): DoubleArgument = DoubleArgument(num)

  private def newRegisterLoad(ref: NodeArgument) = setCurNode(new LoadRegisterNode(ref) with LinearNodeBuilding {
    override val result: Register = fBuilder.newRegister(this)
    override val scope: Function = function
    override val exceptNode: Node = exceptTarget
    override val scopeIndex: Int = fBuilder.nextIdx()
  })

  def finish(): Unit = {
    g.assert()
    g.finish()
    notifyFinish(lastEdges)
  }
}

private trait FunctionBuilding {
  this: Function =>

  var _entryNode: Node = _
  override def entryNode: Node = _entryNode
  override val endNode: Node with NodeBuilding
}

class FunctionBuilder private (name: String, scope: Option[Function]) extends Builder[Function] {

  private val g = new FinishGuard
  private val vars = mutable.ArrayBuffer.empty[LocalVar]
  private val registerList = mutable.ArrayBuffer.empty[Register]
  private var nextScopeIndex = 0

  private[flowgraph] val function: Function with FunctionBuilding = new Function(name, scope) with FunctionBuilding {
    override def parameterNames: IndexedSeq[String] = ???
    override def returnNodes: Seq[Node] = ???
    override val registers: IndexedSeq[Register] = registerList
    override val localVars: IndexedSeq[LocalVar] = vars
    override val endNode: Node with NodeBuilding = new EndNode with NodeBuilding {
      override val scope: Function = function
      override val exceptNode: Node = this
      override def scopeIndex: Int = Int.MaxValue
    }
  }

  val mainBlock = new BlockBuilder(this, function.endNode, None, Seq(entryPseudoEdge), finishMainBlock)

  private[flowgraph] def newVar(varname: String): Int = {
    g.assert()
    val idx = vars.length
    vars += new LocalVar {
      override val index: Int = idx
      override val uid: UId = UId(varname)
      override val scope: Function = function
      override val name: String = varname
    }
    return idx
  }

  private[flowgraph] def newRegister(node: Node): Register = {
    g.assert()
    val register = new Register {
      override val index: Int = registerList.length
      override val uid: UId = UId("reg")
      override val scope: Function = function
      override val definition: Node = node
    }
    registerList += register
    return register
  }

  private[flowgraph] def nextIdx(): Int = {
    val idx = nextScopeIndex
    nextScopeIndex +=1
    idx
  }

  private def entryPseudoEdge = new Edge {
    override def node: Option[Node with NodeBuilding] = None
    override def setNext(node: Node): Unit = {
      g.assert()
      if (function._entryNode != null)
        throw new IllegalStateException("entry node already set. There can only be one entry node!")
      function._entryNode = node
    }
  }

  private def finishMainBlock(openEdges: Seq[Edge]): Unit = {
    // set the end node to all open edges
    openEdges.foreach(_.setNext(function.endNode))
    function.endNode._inNodes ++= openEdges.flatMap(_.node)

    g.finish()
  }

  override def finish(): Function = {
    if(!g.isFinished) {
      mainBlock.finish()
    }
    return function
  }
}


object FunctionBuilder {
  def newProgram(): FunctionBuilder = new FunctionBuilder("<global>", None)
}
