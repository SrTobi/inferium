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

    type Properties = mutable.Map[String, Value]

    class Memory(val idx: Int = 0, val prev: Option[Memory] = None) extends HeapMemory {
        private val objects = mutable.Map.empty[ObjectValue, Properties]
        private var ended = false

        private def set(obj: ObjectValue, property: String, value: Value): Unit = {
            assert(!ended)
            objects.getOrElseUpdate(obj, mutable.Map.empty[String, Value]) += (property -> value)
        }

        private def getHere(obj: ObjectValue, property: String): Option[Value] = {
            return objects.get(obj).flatMap(_.get(property))
        }

        private def getRec(obj: ObjectValue, property: String): Option[Value] = {
            lazy val inPrev = prev.flatMap(_.getRec(obj, property))
            return getHere(obj, property).orElse(inPrev)
        }

        private def get(obj: ObjectValue, property: String): Value = {
            getHere(obj, property) match {
                case Some(value) => value
                case None =>
                    val value = prev.flatMap(_.getRec(obj, property)).getOrElse(UndefinedValue)
                    set(obj, property, value)
                    value
            }
        }

        override def readProperty(target: Value, propertyName: String): Value = {
            val objs = target.asObjects.map(get(_, propertyName))
            return if (objs.isEmpty) UndefinedValue else UnionValue(objs.toSeq: _*)
        }
        override def writeProperty(target: Value, propertyName: String, value: Value): Unit = {
            for(obj <- target.asObjects) {
                set(obj, propertyName, value)
            }
        }

        override def split(): HeapMemory = {
            ended = true
            new Memory(idx + 1, if (objects.isEmpty) prev else Some(this))
        }
    }

    object Memory {

        private class Unifier(var memory: Memory) {
            def index: Int = memory.idx
            val objects = mutable.Map.empty[ObjectValue, Properties]

            def up(): Unit = {
                assert(memory.prev.nonEmpty)
                memory = memory.prev.get
                for (pair@(obj, _) <- memory.objects) {
                    if (!objects.contains(obj)) {
                        objects += pair
                    }
                }
            }

            def merge(otherObjs: mutable.Map[ObjectValue, Properties]): Unit = {
                for (objPropPair@(obj, props) <- otherObjs) {
                    objects.get(obj) match {
                        case Some(properties) =>
                            for (propValuePair@(prop, value) <- props) {
                                properties.get(prop) match {
                                    case Some(value2) =>
                                        properties.update(prop, UnionValue(value, value2))
                                    case None =>
                                        properties += propValuePair
                                }
                            }
                        case None =>
                            objects += objPropPair
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
            }

            memories.foreach(_.ended = true)
            val newIndex = memories.map(_.idx).max + 1
            val unifiers = mutable.Map.empty[Memory, Unifier]
            unifiers ++= memories.map(mem => mem -> new Unifier(mem))

            // the greatest index must be front!
            val queue = mutable.PriorityQueue.empty[Unifier](Ordering.by(_.index))
            queue ++= unifiers.values

            while(true) {
                val unifier = queue.dequeue()

                if (queue.isEmpty) {
                    return new Memory(newIndex, Some(unifier.memory))
                }

                unifiers -= unifier.memory
                unifier.up()
                unifiers.get(unifier.memory) match {
                    case Some(equalUnifier) =>
                        equalUnifier.merge(unifier.objects)
                    case None =>
                        unifiers += (unifier.memory -> unifier)
                        queue += unifier
                }
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