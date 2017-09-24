package com.github.srtobi.inferium.core.flowgraph

import com.github.srtobi.inferium.core.UId

import scala.collection.mutable


abstract class LocalVar {
  def name: String
  def uid: UId
  def index: Int
  def scope: Function

  override def toString: String = s"$$$uid"
}

abstract class Register {
  def definition: Node
  def uid: UId
  def index: Int
  def scope: Function

  override def toString: String = s"%${uid.num}"
}

abstract class Function(val name: String, surroundingScope: Option[Function]) {
  val uid = UId("func")

  val scope: Function = surroundingScope.getOrElse(this)

  val scopeDepth: Int = if (isGlobalScope) 0 else scope.scopeDepth + 1
  def isGlobalScope: Boolean = scope == this
  def scopeByIndex(idx: Int): Function = if (idx > 0) scope.scopeByIndex(idx - 1) else this

  def entryNode: Node
  def returnNodes: Seq[Node]
  def endNode: Node

  def parameterNames: IndexedSeq[String]
  def localVars: IndexedSeq[LocalVar]
  def registers: IndexedSeq[Register]

  override def toString: String = s"$name[${uid.num}]"

  def print(dump: (String) => Unit = println, printExeptTarget: Boolean = false): Unit = {
    var last: Node = null

    def normEnd(node: Node): String = if (node == endNode) "end" else node.scopeIndex.toString

    def printLabel(node: Node): Unit = {
      val in = node.inNodes
      if (in.length > 1 || (in.nonEmpty && in.head != last)) {
        // print a goto before
        val gotoNode = last.outNodes.head
        if (gotoNode != node)
          dump(s"  goto  #${normEnd(gotoNode)}")
        dump(s"${normEnd(node)}:")
      }
      last = node
    }

    def printCmd(cmd: String, node: Node): Unit = {
      if (printExeptTarget) {
        dump(cmd.padTo(40, ' ') + s"[=>${normEnd(node.exceptNode)}]")
      } else {
        dump(cmd)
      }
    }

    dump(s"[[$this]]")
    dump("entry:")
    Function.traverseForward(this) {
      case cond: ConditionalBranchNode =>
        printLabel(cond)
        printCmd(s"  if    ${cond.condition} else jump #${cond.fail.scopeIndex}", cond)
      case node: Node =>
        printLabel(node)
        printCmd(s"  ${node.command.padTo(5, ' ')} ${node.argumentString}", node)
    }
  }
}

object Function {
  def traverseForward(func: Function)(f: (Node) => Unit): Unit = {
    val visited = mutable.HashSet.empty[Int]
    val queue = mutable.PriorityQueue(func.entryNode)(Ordering.by((n:Node) => n.scopeIndex).reverse)

    while (queue.nonEmpty) {
      val node = queue.dequeue()
      queue ++= node.outNodes.filter((n) => !visited.contains(n.scopeIndex))
      visited ++= node.outNodes.map(_.scopeIndex)
      f(node)
    }
  }
}
