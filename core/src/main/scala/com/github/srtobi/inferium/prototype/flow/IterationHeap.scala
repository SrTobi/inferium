package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.collection.mutable


class IterationHeap extends Heap {
    import IterationHeap._





    override def newEmptyHeapState(): HeapMemory = new Memory()
    override def unify(heaps: HeapMemory*): HeapMemory = Memory.unify(heaps.map(_.asInstanceOf[Memory]))
}

object IterationHeap {

    /*case class UnionValue(values: Values) extends ValueLike {
        override def asBool: BoolLattice = {
            assert(values.nonEmpty)
            val it = values.iterator
            var result: BoolLattice = it.next().asBool
            for (bool <- it) {
                result = result.unify(bool.asBool)
                if (result == BoolLattice.Top) {
                    return BoolLattice.Top
                }
            }
            return result
        }
        override def asConcreteValue: Option[ConcreteValue] = {
            assert(values.nonEmpty)
            val it = values.iterator
            return it.next().asConcreteValue.map {
                result =>
                    for (value <- it) {
                        value.asConcreteValue match {
                            case Some(concrete) if concrete == result =>
                                ()
                            case _ =>
                                return None
                        }
                    }
                    result
            }

        }

        override def asSet: mutable.Set[Value] = values.clone()

        override def asFunctions: Traversable[FunctionValue] = values.flatMap(_.asFunctions)
        override def throwsWhenWrittenOrReadOn: Boolean = values.forall(_.throwsWhenWrittenOrReadOn)
    }*/

    type MemoryMap = mutable.Map[ObjectValue, Properties]
    type Properties = mutable.Map[String, (Int, ValueLike)]

    class Memory(val idx: Int = 0, val prev: Option[Memory] = None, private val objects: MemoryMap = mutable.Map.empty) extends HeapMemory {

        private var ended = false
        private var writeCount = 0

        private def nextWriteId(): Int = {
            writeCount += 1
            return writeCount
        }

        private def set(obj: ObjectValue, property: String, value: ValueLike, writeId: Int): Unit = {
            assert(!ended)
            objects.getOrElseUpdate(obj, mutable.Map.empty) += (property -> (writeId, value))
        }

        private[this] def getHere(obj: ObjectValue, property: String): Option[(Int, ValueLike)] = {
            return objects.get(obj).flatMap(_.get(property))
        }

        private def get(obj: ObjectValue, baseObjects: Set[ObjectValue], property: String, cache: Boolean): ValueLike = {
            getHere(obj, property) match {
                case Some((id, value)) =>
                    val overwritingProps = baseObjects.toSeq.flatMap(getHere(_, property)).filter(_._1 > id).map(_._2)
                    if (baseObjects.nonEmpty && overwritingProps.size == baseObjects.size) {
                        UnionValue(overwritingProps: _*)
                    } else {
                        UnionValue(value +: overwritingProps: _*)
                    }
                case None =>
                    val baseProps = baseObjects.flatMap(base => getHere(base, property).map{ case (_, v) => (v, base)})
                    val restBaseObjects = baseObjects -- baseProps.map(_._2)
                    val valuesFromBase = baseProps.map(_._1).toSeq
                    val result = if (restBaseObjects.isEmpty && baseObjects.nonEmpty) {
                        UnionValue(valuesFromBase: _*)
                    } else {
                        val recResult = prev.map(_.get(obj, restBaseObjects, property, cache = false)).getOrElse(UndefinedValue)
                        UnionValue(recResult +: valuesFromBase: _*)
                    }
                    if (cache)
                        set(obj, property, result, -1)
                    result
            }
        }

        def readProperty(target: ValueLike, property: String, cache: Boolean): ValueLike = {
            val result = target.asObject.map(get(_, target.baseObjects.toSet, property, cache = cache)).getOrElse(UndefinedValue)
            if (target.propertyWriteMaybeNoOp) UnionValue.withUndefined(result) else result
        }

        override def readProperty(target: ValueLike, propertyName: String): ValueLike = {
            filterUnwritables(target)
            readProperty(target, propertyName, cache = true)
        }
        override def writeProperty(target: ValueLike, propertyName: String, value: ValueLike): Unit = {
            filterUnwritables(target)
            val writeId = nextWriteId()
            target.asObject.foreach {
                set(_, propertyName, value, writeId)
            }

            target.baseObjects foreach {
                obj =>
                    set(obj, propertyName, UnionValue(readProperty(obj, propertyName), value), writeId)
            }
        }

        private def filterUnwritables(target: ValueLike): Unit = target match {
            case ref: Reference =>
                manipulateReference(ref, (value) => {
                    value.withoutThrowingWhenWrittenOn
                })
            case _ =>
        }

        private def manipulateReference(ref: Reference, manipulate: (ValueLike) => ValueLike): Unit = ref match {
            case Reference(value, obj, property) =>
                val org = readProperty(obj, property, cache = false)
                if (org == value) {
                    writeProperty(obj, property, manipulate(org))
                }
        }

        override def split(): HeapMemory = {
            ended = true
            new Memory(idx + 1, if (objects.isEmpty) prev else Some(this))
        }

        override def toString: String = s"Heap[$idx]"
    }

    object Memory {

        private class Unifier(iniMemory: Memory) {
            var memory: Memory = iniMemory.prev.get
            def index: Int = memory.idx
            val objects: MemoryMap = iniMemory.objects.map { case (prop, value) => (prop, value.clone())}

            def up(): Unit = {
                assert(memory.prev.nonEmpty)
                for (pair@(obj, _) <- memory.objects) {
                    if (!objects.contains(obj)) {
                        objects += pair
                    }
                }
                memory = memory.prev.get
            }

            def merge(other: Unifier): Unit = {
                val objSet = objects.keySet ++ other.objects.keySet

                objSet.foreach {
                    obj =>
                        val aProps = objects.getOrElseUpdate(obj, mutable.Map())
                        val bProps = other.objects.getOrElse(obj, mutable.Map())
                        val props = aProps.keySet ++ bProps.keySet
                        props.foreach {
                            prop =>
                                lazy val default = memory.readProperty(obj, prop, cache = false)
                                val aVal = aProps.get(prop).map(_._2).getOrElse(default)
                                val bVal = bProps.get(prop).map(_._2).getOrElse(default)
                                aProps.update(prop, (0, UnionValue(aVal, bVal)))
                        }
                }
            }
        }

        def unify(memories: Seq[Memory]): Memory = {
            assert(memories.nonEmpty)
            assert(memories.distinct.length == memories.length)

            memories match {
                case Seq(mem) =>
                    return mem
                case _ =>
            }

            memories.foreach(_.ended = true)
            val newIndex = memories.map(_.idx).max + 1
            val unifiers = mutable.Map.empty[Memory, Unifier]
            // the greatest index must be front!
            val queue = mutable.PriorityQueue.empty[Unifier](Ordering.by(_.index))

            def addToUnifiers(unifier: Unifier): Unit = {
                unifiers.get(unifier.memory) match {
                    case Some(equalUnifier) =>
                        equalUnifier.merge(unifier)
                    case None =>
                        unifiers += (unifier.memory -> unifier)
                        queue += unifier
                }
            }

            memories.map(new Unifier(_)).foreach(addToUnifiers)

            while(true) {
                val unifier = queue.dequeue()

                if (queue.isEmpty) {
                    return new Memory(newIndex, Some(unifier.memory), unifier.objects)
                }

                unifiers -= unifier.memory
                unifier.up()
                addToUnifiers(unifier)
            }

            throw new IllegalStateException("should not be reached")
        }
    }
}

/*
import com.github.srtobi.inferium.prototype.flow.Heap.ValueHandleChangeHandler

import scala.collection.mutable
class IterationHeap(solver: Solver) extends HeapState {

    private val unprocessedInflow = mutable.Queue.empty[(HeapNode, HeapNode)]

    private class Handle extends HeapState.ValueHandle

    private class Memory(val idx: Int = 0, val prev: Option[Memory] = None) {
        private val handleValue = mutable.Map.empty[HeapState.ValueHandle, Value]
        private var ended = false

        def set(handle: HeapState.ValueHandle, value: Value): Unit = {
            assert(!ended)
            handleValue += (handle -> value)
        }

        def get(handle: HeapState.ValueHandle): Value = {
            return handleValue.getOrElseUpdate(handle, prev.flatMap(p => p.getImpl(handle)).getOrElse(solver.undefined()))
        }

        private def getImpl(handle: HeapState.ValueHandle): Option[Value] = {
            return handleValue.get(handle).map(Some(_)).getOrElse(prev.flatMap(p => p.getImpl(handle)))
        }

        def split(): Memory =  {
            ended = true
            new Memory(idx + 1, if (handleValue.isEmpty) prev else Some(this))
        }

        def unify(other: Memory): Memory = {
            other.ended = true
            if (other == this)
                return this
            this.ended = true

            val handles = mutable.Set.empty[HeapState.ValueHandle]
            var (a, b) = (this, other)
            val newIdx = 1 + Math.max(a.idx, b.idx)

            while (a ne b) {
                if (a.idx < b.idx) {
                    val tmp = a
                    a = b
                    b = tmp
                }
                handles ++= a.handleValue.keys
                assert(a.prev.nonEmpty)
                a = a.prev.get
            }

            val newmem = new Memory(newIdx, Some(a))

            handles.foreach(h => {
                val aval = a.getImpl(h).getOrElse(solver.undefined())
                val bval = b.getImpl(h).getOrElse(solver.undefined())
                newmem.set(h, solver.unify(aval, bval))
            })

            return newmem
        }
    }

    private sealed abstract class HeapNode {
        val next: mutable.Buffer[HeapNode] = mutable.Buffer.empty[HeapNode]
        val prev: mutable.Buffer[HeapNode] = mutable.Buffer.empty[HeapNode]

        def flowTo(other: HeapNode): Unit = {
            next += other
            other.prev += this
        }
    }

    private class NothingNode extends HeapNode

    private class WriteNode(val handle: Handle) extends HeapNode with HeapState.HandleWriter {
        var value: Option[Value] = None
        override def write(value: Value): Unit = this.value = Some(value)
    }

    private class ReadNode(val handle: Handle, handler: ValueHandleChangeHandler) extends HeapNode with HeapState.HandleReader {
        private var value: Value = _
        override def read(): Value = value

        def supply(newval: Value): Boolean = {
            if (value ne newval) {
                val oldval = Option(value)
                value = newval
                handler.onHandleChanged(oldval, newval)
                return true
            } else {
                return false
            }
        }
    }

    private class MergeNode extends HeapNode with HeapState.ValueHandleMerger {
        val handles = mutable.Set.empty[Handle]
        override def add(handles: HeapState.ValueHandle*): Unit = this.handles ++= handles.map(_.asInstanceOf[Handle])
    }


    private class State(val node: HeapNode) extends HeapState.State with HeapState.MergeState {
        override def newHandleReader(handle: HeapState.ValueHandle, changeHandler: ValueHandleChangeHandler): (HeapState.HandleReader, HeapState.State) = {
            val h = handle.asInstanceOf[Handle]
            val reader = new ReadNode(h, changeHandler)
            unprocessedInflow += (node -> reader)
            return (reader, new State(reader))
        }

        override def newHandleWriter(handle: HeapState.ValueHandle): (HeapState.HandleWriter, HeapState.State) = {
            val h = handle.asInstanceOf[Handle]
            val writer = new WriteNode(h)
            unprocessedInflow += (node -> writer)
            return (writer, new State(writer))
        }

        override def newValueHandleMerger(): (HeapState.ValueHandleMerger, HeapState.State) = {
            val merger = new MergeNode
            unprocessedInflow += (node -> merger)
            return (merger, new State(merger))
        }

        override def truthyfy(cond: HeapState.ValueHandle): HeapState.State = ???
        override def falsyfy(cond: HeapState.ValueHandle): HeapState.State = ???

        override def addInflow(heapState: HeapState.State): Unit = {
            val pre = heapState.asInstanceOf[State].node
            unprocessedInflow += (pre -> node)
        }
    }




    override def newEmptyHeapState(): HeapState.State = new State(new NothingNode)
    override def newMergeHeapState(numTip: Int): HeapState.MergeState = new State(new NothingNode)
    override def newValueHandle(): HeapState.ValueHandle = new Handle

    override def propagateFlow(startHeap: HeapState.State): Boolean = {


        while (unprocessedInflow.nonEmpty) {
            val (from, to) = unprocessedInflow.dequeue()
            from.flowTo(to)
        }


        val queue = mutable.Queue.empty[(HeapNode, Memory)]

        val waitingHeaps = mutable.HashMap.empty[HeapNode, (Memory, Int)]
        val start = startHeap.asInstanceOf[HeapNode]
        queue += (start -> new Memory())
        var changed = false
        while (queue.nonEmpty) {
            val (state, mem) = queue.dequeue()

            val process = waitingHeaps.get(state) match {
                case Some((mergedMem, remaining)) =>
                    if (remaining <= 1) {
                        waitingHeaps.remove(state)
                        mergedMem.unify(mem)
                        true
                    } else {
                        waitingHeaps.update(state, (mergedMem.unify(mem), remaining - 1))
                        false
                    }
                case None =>
                    val numIns = state.prev.size
                    if (numIns > 1) {
                        waitingHeaps.update(state, (mem, numIns - 1))
                        false
                    } else {
                        true
                    }
            }

            if (process) {
                state match {
                    case _: NothingNode => ()
                    case node: WriteNode =>
                        node.value.foreach {
                            value =>
                                mem.set(node.handle, value)
                        }
                    case node: ReadNode =>
                        changed = node.supply(mem.get(node.handle)) || changed
                    case node: MergeNode =>
                        mem.set(node, solver.unify(node.handles.toSeq.map(mem.get): _*))
                }

                val split = state.next.size >= 2
                for (next <- state.next) {
                    queue += ((next, if (split) mem.split() else mem))
                }
            }
        }

        return changed
    }
}
*/