package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap.ValueHandleChangeHandler

import scala.collection.mutable

class IterationHeap(solver: Solver) extends Heap {

    private val unprocessedInflow = mutable.Queue.empty[(HeapNode, HeapNode)]

    private class Handle extends Heap.ValueHandle

    private class Memory(val idx: Int = 0, val prev: Option[Memory] = None) {
        private val handleValue = mutable.Map.empty[Heap.ValueHandle, Value]
        private var ended = false

        def set(handle: Heap.ValueHandle, value: Value): Unit = {
            assert(!ended)
            handleValue += (handle -> value)
        }

        def get(handle: Heap.ValueHandle): Value = {
            return handleValue.getOrElseUpdate(handle, prev.flatMap(p => p.getImpl(handle)).getOrElse(solver.undefined()))
        }

        private def getImpl(handle: Heap.ValueHandle): Option[Value] = {
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

            val handles = mutable.Set.empty[Heap.ValueHandle]
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

    private class WriteNode(val handle: Handle) extends HeapNode with Heap.HandleWriter {
        var value: Option[Value] = None
        override def write(value: Value): Unit = this.value = Some(value)
    }

    private class ReadNode(val handle: Handle, handler: ValueHandleChangeHandler) extends HeapNode with Heap.HandleReader {
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

    private class MergeNode extends HeapNode with Heap.ValueHandleMerger {
        val handles = mutable.Set.empty[Handle]
        override def add(handles: Heap.ValueHandle*): Unit = this.handles ++= handles.map(_.asInstanceOf[Handle])
    }


    private class State(val node: HeapNode) extends Heap.State with Heap.MergeState {
        override def newHandleReader(handle: Heap.ValueHandle, changeHandler: ValueHandleChangeHandler): (Heap.HandleReader, Heap.State) = {
            val h = handle.asInstanceOf[Handle]
            val reader = new ReadNode(h, changeHandler)
            unprocessedInflow += (node -> reader)
            return (reader, new State(reader))
        }

        override def newHandleWriter(handle: Heap.ValueHandle): (Heap.HandleWriter, Heap.State) = {
            val h = handle.asInstanceOf[Handle]
            val writer = new WriteNode(h)
            unprocessedInflow += (node -> writer)
            return (writer, new State(writer))
        }

        override def newValueHandleMerger(): (Heap.ValueHandleMerger, Heap.State) = {
            val merger = new MergeNode
            unprocessedInflow += (node -> merger)
            return (merger, new State(merger))
        }

        override def truthyfy(cond: Heap.ValueHandle): Heap.State = ???
        override def falsyfy(cond: Heap.ValueHandle): Heap.State = ???

        override def addInflow(heapState: Heap.State): Unit = {
            val pre = heapState.asInstanceOf[State].node
            unprocessedInflow += (pre -> node)
        }
    }




    override def newEmptyHeapState(): Heap.State = new State(new NothingNode)
    override def newMergeHeapState(numTip: Int): Heap.MergeState = new State(new NothingNode)
    override def newValueHandle(): Heap.ValueHandle = new Handle

    override def propagateFlow(startHeap: Heap.State): Boolean = {


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
