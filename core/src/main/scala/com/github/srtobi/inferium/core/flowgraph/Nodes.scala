package com.github.srtobi.inferium.core.flowgraph

import com.github.srtobi.inferium.core.UId

sealed abstract class NodeArgument

sealed abstract class LocalVarRef extends NodeArgument {
  def name: String
}

case class ResolvedLocalVarRef(scopeIdx: Int, localVarIdx: Int)(val scope: Function) extends LocalVarRef {
  def target: LocalVar = scope.localVars(localVarIdx)

  override def name: String = target.name
  override def toString: String = target.toString
}

case class UnresolvedLocalVarRef(override val name: String) extends LocalVarRef {
  override def toString: String = s"$name?"
}

sealed abstract class AvailableArgument extends NodeArgument

case class RegisterRef(target: Register) extends AvailableArgument {
  override def toString: String = target.toString
}

sealed abstract class ValueArgument extends AvailableArgument

case class StringArgument(value: String) extends ValueArgument {
  override def toString: String = s"'$value'"
}

case class IntegerArgument(value: Long) extends ValueArgument {
  override def toString: String = value.toString
}

case class DoubleArgument(value: Double) extends ValueArgument {
  override def toString: String = value.toString
}


sealed abstract class Node {
  def name: String
  val uid: UId = UId(name)

  def inNodes: Seq[Node]
  def outNodes: Seq[Node]
  def exceptNode: Node

  def scope: Function

  def comments: Seq[String]
  def scopeIndex: Int
  def command: String
  def arguments: Seq[NodeArgument]
  def argumentString: String = arguments.mkString(", ")

  override def toString: String = s"$uid@$scopeIndex"
}

object Node {
  def isBackwardEdge(from: Node, to: Node): Boolean = from.scopeIndex >= to.scopeIndex
}

abstract sealed class LinearNode extends Node {
  override def outNodes: Seq[Node] = Seq(next)
  def next: Node
}

abstract case class LoadRegisterNode(src: NodeArgument) extends LinearNode {
  override def name: String = "LoadRegister"
  override def command: String = "mov"
  override lazy val arguments: Seq[NodeArgument] = Seq(RegisterRef(result), src)

  override def argumentString: String = s"$result <- $src"

  def result: Register
}

abstract case class WriteLocalVarNode(dest: LocalVarRef, src: NodeArgument) extends LinearNode {
  override def name: String = "WriteLocal"
  override def command: String = "mov"
  override def arguments: Seq[NodeArgument] = Seq(dest, src)
  override def argumentString: String = s"$dest <- $src"

  def next: Node
}

abstract case class InitRegisterNode()

abstract case class ReadPropertyNode(base: NodeArgument, property: NodeArgument) extends LinearNode {
  override def name: String = "ReadProperty"
  override def command: String = "read"
  override lazy val arguments: Seq[NodeArgument] = Seq(RegisterRef(result), base, property)
  override def argumentString: String = s"$result <- $base.$property"

  def result: Register
}

abstract case class WritePropertyNode(base: NodeArgument, property: NodeArgument, src: NodeArgument) extends LinearNode {
  override def name: String = "WriteProperty"
  override def command: String = "write"
  override def arguments: Seq[NodeArgument] = Seq(base, property, src)
  override def argumentString: String = s"$base.$property <- $src"
}

abstract case class ConditionalBranchNode(condition: NodeArgument) extends Node {
  override def name: String = "ConditionalBranch"
  override def command: String = "if"

  override def arguments: Seq[NodeArgument] = Seq(condition)
  override def outNodes: Seq[Node] = Seq(success, fail)

  def success: Node
  def fail: Node
}

class UnaryInternalOperation
class BinaryInternalOperation

abstract case class UnaryOperationNode(operation: UnaryInternalOperation, argument: NodeArgument) extends LinearNode {
  override def name: String = s"UnaryOperation[$operation]"
  override def command: String = operation.toString

  override def arguments: Seq[NodeArgument] = Seq(argument)
}

abstract case class BinaryOperationNode(operation: BinaryInternalOperation, left: NodeArgument, right: NodeArgument) extends LinearNode {
  override def name: String = s"BinaryOperation[$operation]"
  override def command: String = operation.toString

  override def arguments: Seq[NodeArgument] = Seq(left, right)
}

abstract case class CallNode(function: RegisterRef, argumentList: RegisterRef) extends LinearNode {
  override def name: String = "Call"
  override def command: String = "call"

  override def arguments: Seq[NodeArgument] = Seq(argumentList)
}

abstract case class InitFunctionNode(function: Function) extends LinearNode {
  override def name: String = "InitFunction"
  override def command: String = "init"

  override def arguments: Seq[NodeArgument] = Seq()
  override def argumentString: String = function.toString
  def result: Register
}

abstract case class ReturnNode(returnValue: Option[NodeArgument]) extends Node {
  override def name: String = "Return"
  override def command: String = "ret"

  override def arguments: Seq[NodeArgument] = returnValue.toSeq
}

abstract case class EndNode() extends Node {
  override def name: String = "End"
  override def command: String = "end"

  override def outNodes: Seq[Node] = Seq()
  override def arguments: Seq[NodeArgument] = Seq()
}