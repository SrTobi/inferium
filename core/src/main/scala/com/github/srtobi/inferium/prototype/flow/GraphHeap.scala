package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap.{ValueHandle, ValueHandleChangeHandler}

import scala.collection.mutable
/*
class GraphHeap(solver: Solver) extends Heap {

    class Handle extends ValueHandle {
    }

    private class NodeAction {
        var write: Option[ValueSlot] = None
        val reads: mutable.Map[ValueSlot, HReader] = mutable.Map.empty[ValueSlot, HReader]
    }

    private class ValueSlot {
        private var _value: Value = _
        val changeHandlers: mutable.Set[ValueHandleChangeHandler] = mutable.Set.empty[ValueHandleChangeHandler]

        // registerValueDemand(...)

        def value: Value = _value
        def value_=(newVal: Value): Unit = {
            if (newVal != value) {
                // registerValueSlotChange(this, value, newVal)
            }
        }

        def needsValue: Boolean = _value == null
    }

    sealed abstract class HeapNode(var num: Int) {
        protected val _next: mutable.Buffer[HeapNode] = mutable.Buffer.empty[HeapNode]

        val actions: mutable.Map[Handle, NodeAction] = mutable.Map.empty

        def action(handle: Handle): NodeAction = actions.getOrElse(handle, new NodeAction)

        def makeHReader(handle: Handle, handler: ValueHandleChangeHandler): HReader = {
            val a = action(handle)
            return new HReader(handle, this, handler)
        }

        def makeHWriter(handle: Handle): HWriter = {
            val a = action(handle)
            val slot = new ValueSlot
            a.write = Some(slot)
            return new HWriter(handle, slot)
        }

        def makePredecessor(): HeapNode = {
            val node = new NormalHeapNode(num + 1, this)
            this._next += node
            return node
        }
    }

    final class StartNode extends HeapNode(0)

    final class NormalHeapNode(num: Int, val prev: HeapNode) extends HeapNode(num)

    final class MergeHeapNode(num: Int) extends HeapNode(num) {
        val prev: mutable.Buffer[HeapNode] = mutable.Buffer.empty[HeapNode]
    }

    class HReader(val handle: Handle, val node: HeapNode, handler: ValueHandleChangeHandler) extends Heap.HandleReader {
        private var _slot: ValueSlot = _
        def slot: ValueSlot = _slot
        def slot_=(newSlot: ValueSlot): Unit = {
            newSlot.changeHandlers += handler
            // registerValueSlotChange(handler, slot.value, newSlot.value)
        }

        override def read(): Value = slot.value
    }

    class HWriter(val handle: Handle, val slot: ValueSlot) extends Heap.HandleWriter {
        override def write(value: Value): Unit = {
            slot.value = value
        }
    }
    
    /*case class PropertyNode(property: String)(num: Int) extends HeapNode(num) {
        val propertyActions = mutable.Map.empty[Value, NodeAction]
        
        override def equals(o: scala.Any): Boolean = super.equals(o)
    }*/


    class State(val node: HeapNode) extends Heap.State {
        override def newHandleReader(handle: Heap.ValueHandle, changeHandler: ValueHandleChangeHandler): (Heap.HandleReader, Heap.State) = {
            val h = handle.asInstanceOf[Handle]
            return (node.makeHReader(h, changeHandler), this)
        }
        
        override def newHandleWriter(handle: Heap.ValueHandle): (Heap.HandleWriter, Heap.State) = {
            val h = handle.asInstanceOf[Handle]
            val next = node.makePredecessor()
            return (next.makeHWriter(h), new State(next))
        }

        override def newValueHandleMerger(): (Heap.ValueHandleMerger, Heap.State) = {
            val next = node.makePredecessor()
            return (new HandleMerger(node, next), new State(next))
        }

        override def truthyfy(cond: Heap.ValueHandle): Heap.State = ???
        override def falsyfy(cond: Heap.ValueHandle): Heap.State = ???
    }

    class MergeState(override val node: MergeHeapNode) extends State(node) with Heap.MergeState {
        override def addInflow(heapState: Heap.State): Unit = {
            // register flow change
        }
    }


    class HandleMerger(readNode: HeapNode, writeNode: HeapNode) extends Handle with Heap.ValueHandleMerger with Heap.ValueHandleChangeHandler {
        private val readers = mutable.Map.empty[ValueHandle, HReader]
        private val writer = writeNode.makeHWriter(this)
        private val union = solver.union()

        override def add(handles: ValueHandle*): Unit = {
            for (handle <- handles.map(_.asInstanceOf[Handle])) {
                readers.getOrElseUpdate(handle, readNode.makeHReader(handle, this))
            }
        }

        override def onHandleChanged(oldValue: Option[Value], newValue: Value): Unit = {
            // TODO: remove old inflow but the old value could come from multiple readers
            //oldValue.foreach(_.removeFlowTo(union))

            newValue.flowsTo(union)
            if (readers.size > 1) {
                writer.write(union)
            } else {
                writer.write(newValue)
            }
        }
    }

    override def newEmptyHeapState(): Heap.State = new State(new StartNode)

    override def newMergeHeapState(numTip: Int): Heap.MergeState = new MergeState(new MergeHeapNode(0))

    override def newValueHandle(): Heap.ValueHandle = new Handle


    override def propagateFlow(startHeap: Heap.State): Boolean = {
        return ???
    }
}
*/