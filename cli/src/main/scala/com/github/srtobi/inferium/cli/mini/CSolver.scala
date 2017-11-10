package com.github.srtobi.inferium.cli.mini

import com.github.srtobi.inferium.core.UId

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


class NodeContext(solver: CSolver, val outer: Option[NodeContext]) {
  val id: UId = UId("context")
  val allInnerContexts = mutable.HashSet.empty[NodeContext]

  def isInnerContext(nodeContext: NodeContext): Boolean = allInnerContexts(nodeContext)
  def isOuterContext(nodeContext: NodeContext): Boolean = nodeContext != this && !isInnerContext(nodeContext)
  private def registerInOuter(nodeContext: NodeContext): Unit = {
    outer.foreach {
      outer =>
        outer.allInnerContexts.add(nodeContext)
        outer.registerInOuter(nodeContext)
    }
  }
  registerInOuter(this)

  override def equals(o: Any): Boolean = o match {
    case ref: AnyRef => this eq ref
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
}

abstract class ConstraintNode(val solver: CSolver, idGroup: String) {
  val id: UId = UId(idGroup)
  val context: NodeContext = solver.curContext
  var mark: Boolean = false

  def marked(): this.type = { mark = true; this}

  override def equals(o: Any): Boolean = o match {
    case ref: AnyRef => this eq ref
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
}


abstract class SpecificConstraintNode[NodeType <: SpecificConstraintNode[NodeType]](solver: CSolver, idGroup: String) extends ConstraintNode(solver, idGroup) {
  this: NodeType =>
  private val _in = mutable.HashSet.empty[NodeType]
  private val _out = mutable.HashSet.empty[NodeType]

  def in: scala.collection.Set[NodeType] = _in
  def out: scala.collection.Set[NodeType] = _out

  def flowsTo(to: NodeType): Boolean = {
    val wasAdded = connectTo(to)
    if (wasAdded) {
      this.onFlowsOutTo(to)
      to.onFlowsInFrom(this)
    }
    return wasAdded
  }
  def connectTo(to: NodeType): Boolean = {
    if (to == this)
      return false

    val wasAdded = _out.add(to)
    if (wasAdded) {
      to._in.add(this)
    }
    return wasAdded
  }

  def onFlowsOutTo(to: NodeType): Unit = ()
  def onFlowsInFrom(from: NodeType): Unit = ()
}


// --------------------------- Types ---------------------------------

abstract class TypeUsage(solver: CSolver, idGroup: String) extends SpecificConstraintNode[TypeUsage](solver, idGroup) {
  def name: String
  def fencing: Boolean
}

class TypeVariable(solver: CSolver) extends TypeUsage(solver, "TypeVar") {
  override def name: String = "$" + id.num

  override def toString: String = name

  override def onFlowsOutTo(to: TypeUsage): Unit = solver.addTypeFlow(this, to)

  override def fencing: Boolean = false
}

class FunctionType(solver: CSolver, val args: Seq[TypeVariable], val ret: TypeVariable, val scopes: Seq[TypeVariable], val heapBegin: HeapNode, val heapEnd: HeapNode) extends TypeVariable(solver) {
  override def name: String = s"func#${id.num}"

  override def fencing: Boolean = true
  mark = true
}

class ConcreteType(solver: CSolver, override val name: String) extends TypeVariable(solver) {
  override def fencing: Boolean = true
  mark = true
}

class ObjectRefType(solver: CSolver) extends TypeVariable(solver)  {
  override def name: String = s"obj#${id.num}"
  override def fencing: Boolean = true
}

class TypeAcceptor(solver: CSolver) extends TypeUsage(solver, "TypeAcceptor") {
  override def name: String = s"acc#${id.num}"
  override def fencing: Boolean = true
}

// --------------------------- Effects ---------------------------------


// --------------------------- Heap ---------------------------------
class HeapNode(solver: CSolver) extends SpecificConstraintNode[HeapNode](solver, "Heap")
class NewObjectHeap(solver: CSolver) extends HeapNode(solver) {
  val newObjectRefType: ObjectRefType = new ObjectRefType(solver)
}
class WriteObjectPropertyHeap(solver: CSolver, val targetObject: TypeVariable, val property: String, val value: TypeVariable) extends HeapNode(solver) {
}
class ReadObjectPropertyHeap(solver: CSolver, val targetObject: TypeVariable, val property: String) extends HeapNode(solver) {
  val resultType: TypeVariable = new TypeVariable(solver)
}
class CallSite(solver: CSolver, val function: TypeVariable, val arguments: Seq[TypeVariable], val nextHeap: HeapNode) extends HeapNode(solver) {
  solver.registerCallSite(this)

  val returnType: TypeVariable = new TypeVariable(solver)
}

class CSolver {
  private val heapStarts = mutable.ArrayBuffer.empty[HeapNode]
  private val callSites = mutable.ArrayBuffer.empty[CallSite]
  private val contextStack = mutable.Stack[NodeContext]()

  def curContext: NodeContext = contextStack.top
  def pushContext(nodeContext: NodeContext): Unit = contextStack.push(nodeContext)
  def popContext(): Unit = contextStack.pop()

  def registerHeapStart(heapNode: HeapNode): Unit = heapStarts += heapNode
  def registerCallSite(callSite: CallSite): Unit = callSites += callSite

  def addTypeFlow(from: TypeVariable, to: TypeUsage): Unit = {
    // propagate flow
    if (!from.fencing) {
      from.in.foreach(_.connectTo(to))
    }

    if (!to.fencing) {
      to.out.foreach(from.connectTo)
    }

    if (!from.fencing && !to.fencing) {
      from.in.foreach(in => to.out.foreach(in.connectTo))
    }
  }

  def solve(): Unit = {
    var changed = false
    do {
      changed = false
      heapStarts.foreach(changed ||= propagateHeap(_))
    } while (changed)
  }

  private def propagateHeap(start: HeapNode): Boolean = {
    type Heap = mutable.HashMap[ObjectRefType, mutable.HashMap[String, mutable.Set[TypeVariable]]]

    def cloneHeap(h: Heap): Heap = {
      val newHeap: Heap = mutable.HashMap.empty
      h.foreach {
        case (ref, properties) =>
          val newMap = mutable.HashMap.empty[String, mutable.Set[TypeVariable]]
          newHeap.update(ref, newMap)
          properties.foreach {
            case (prop, vars) =>
              newMap.update(prop, vars.clone())
          }
      }
      newHeap
    }

    def mergeHeaps(into: Heap, other: Heap): Heap = {
      other.foreach {
        case (ref, properties) =>
          val intoProps = into.getOrElseUpdate(ref, mutable.HashMap.empty)
          properties.foreach {
            case (prop, vars) =>
              intoProps.getOrElseUpdate(prop, mutable.HashSet.empty) ++= vars
          }
      }
      into
    }

    val startHeap: Heap = mutable.HashMap.empty
    val waitingHeaps = mutable.HashMap.empty[HeapNode, (Heap, Int)]
    val queue = mutable.Queue((startHeap, start))
    var changed = false
    while (queue.nonEmpty) {
      val (heap, cur) = queue.dequeue()
      val process = waitingHeaps.get(cur) match {
        case Some((h, remaining)) =>
          if (remaining <= 1) {
            waitingHeaps.remove(cur)
            mergeHeaps(heap, h)
            true
          } else {
            waitingHeaps.update(cur, (mergeHeaps(heap, h), remaining - 1))
            false
          }
        case None =>
          val numIns = cur.in.size
          if (numIns > 1) {
            waitingHeaps.update(cur, (heap, numIns - 1))
            false
          } else {
            true
          }
      }
      if (process) {
        cur match {
          case read: ReadObjectPropertyHeap =>
            val targets = (read.targetObject #:: read.targetObject.in.toStream).collect{case ref: ObjectRefType => ref}
            val inflow = targets.flatMap(heap.get).flatMap(_.get(read.property)).flatten
            inflow.foreach(changed ||= _.flowsTo(read.resultType))
          case write: WriteObjectPropertyHeap =>
            val targets = (write.targetObject #:: write.targetObject.in.toStream).collect{case ref: ObjectRefType => ref}
            val outflow = targets
              .map(heap.getOrElseUpdate(_, mutable.HashMap.empty))
              .map(_.getOrElseUpdate(write.property, mutable.HashSet.empty))
            outflow.foreach{slot => slot.clear(); slot += write.value}
          case callSite: CallSite =>
            expandCallSite(callSite)
          case _ => ()
        }
        // CallSites are not allowed to have outs
        assert(!cur.isInstanceOf[CallSite] || cur.out.isEmpty)
        queue ++= (heap #:: Stream.continually(cloneHeap(heap))).zip(cur.out)
      }
    }
    assert(waitingHeaps.isEmpty)
    return changed
  }

  private def expandCallSite(callSite: CallSite): Boolean = {
    return false
  }

  /*
  def addHeapFlow(from: HeapNode, to: HeapNode): Unit = {

  }

  def addWriteTarget(writer: WriteObjectPropertyHeap, newTarget: ObjectRefType): Unit = {
    // the writer has a new target
    // search up the heap chain and find readers
    val property = writer.property
    val queue = mutable.Queue[HeapNode](writer.out.toSeq: _*)

    while (queue.nonEmpty) {
      val cur = queue.dequeue()
      cur match {
        case reader: ReadObjectPropertyHeap =>
          if (reader.targetObject.in.contains(newTarget)) {

          }
      }
    }
  }*/
}
