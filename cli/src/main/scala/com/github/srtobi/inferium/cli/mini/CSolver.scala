package com.github.srtobi.inferium.cli.mini

import com.github.srtobi.inferium.core.UId

import scala.collection.mutable


abstract class ConstraintNode(idGroup: String) {
  val id: UId = UId(idGroup)

  override def equals(o: Any): Boolean = o match {
    case ref: AnyRef => this eq ref
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
}

package NodeImpl {

  trait InNode[InType <: ConstraintNode with InNode[InType, OutType], OutType <: ConstraintNode with OutNode[OutType, InType]] {
    private[NodeImpl] val _in = mutable.HashSet.empty[OutType]

    def in: scala.collection.Set[OutType] = _in
  }

  trait OutNode[OutType <: ConstraintNode with OutNode[OutType, InType], InType <: ConstraintNode with InNode[InType, OutType]] {
    this: OutType =>
    private val _out = mutable.HashSet.empty[InType]

    def out: scala.collection.Set[InType] = _out

    def flowsTo(to: InType): Boolean = {
      val wasAdded = _out.add(to)
      if (wasAdded) {
        to._in.add(this)
      }
      return wasAdded
    }
  }

}

// --------------------------- Types ---------------------------------

abstract class TypeUsage(idGroup: String) extends ConstraintNode(idGroup) with NodeImpl.InNode[TypeUsage, TypeVariable] {
  def name: String
}

class TypeVariable extends TypeUsage("TypeVar") with NodeImpl.OutNode[TypeVariable, TypeUsage] {
  override def name: String = "$" + id.num

  override def toString: String = name
}

class FunctionType(val args: Seq[TypeVariable], val ret: TypeVariable, val scopes: Seq[TypeVariable], val heapBegin: HeapNode, val heapEnd: HeapNode) extends TypeVariable {
  override def name: String = s"func#${id.num}"
}

class ConcreteType(override val name: String) extends TypeVariable

class ObjectRefType extends TypeVariable {
  override def name: String = s"obj#${id.num}"
}

// --------------------------- Effects ---------------------------------


// --------------------------- Heap ---------------------------------
class HeapNode extends ConstraintNode("Heap") with NodeImpl.OutNode[HeapNode, HeapNode] with NodeImpl.InNode[HeapNode, HeapNode]
class NewObjectHeap extends HeapNode {
  val newObjectRefType: ObjectRefType = new ObjectRefType()
}
class WriteObjectPropertyHeap(val targetObject: TypeUsage, val property: String, val value: TypeUsage) extends HeapNode
class ReadObjectPropertyHeap(val targetObject: TypeUsage, val property: String) extends HeapNode {
  val resultType: TypeVariable = new TypeVariable
}

class CSolver {
  def add(from: TypeVariable, to: TypeUsage): Unit = ???
}
