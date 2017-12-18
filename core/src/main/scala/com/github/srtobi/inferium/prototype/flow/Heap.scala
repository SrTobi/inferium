package com.github.srtobi.inferium.prototype.flow

import Nodes.Node


class HeapStateBuilder(private var cur: Heap.State, private val node: Node) {
    def newHandleReader(handle: Heap.ValueHandle, handler: Heap.ValueHandleChangeHandler): Heap.HandleReader = {
        assert(isBuilding)
        val (reader, newHeap) = cur.newHandleReader(handle, handler)
        cur = newHeap
        return reader
    }

    def newHandleWriter(handle: Heap.ValueHandle): Heap.HandleWriter = {
        assert(isBuilding)
        val (writer, newHeap) = cur.newHandleWriter(handle)
        cur = newHeap
        return writer
    }

    def newHandleMerge(): Heap.ValueHandleMerger = {
        assert(isBuilding)
        val (merger, newHeap) = cur.newValueHandleMerger()
        cur = newHeap
        return merger
    }

    /*def readProperty(target: Heap.ValueHandle, property: String, into: Heap.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.readProperty(target, property, into)
    }

    def writeProperty(target: Heap.ValueHandle, property: String, value: Heap.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.writeProperty(target, property, value)
    }

    def readLocal(target: EmptyObject, property: String, into: Heap.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.readLocal(target, property, into)
    }

    def writeLocal(target: EmptyObject, property: String, value: Heap.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.writeLocal(target, property, value)
    }*/

    def end(): Heap.State = {
        if (cur == null) {
            throw new IllegalStateException("Can not end a builder a second time")
        }
        val result = cur
        cur = null
        return result
    }

    def isBuilding: Boolean = !hasEnded
    def hasEnded: Boolean = cur == null
}

trait Heap {
    def newEmptyHeapState(): Heap.State
    def newMergeHeapState(numTip: Int = 0): Heap.MergeState
    def newValueHandle(): Heap.ValueHandle

    def propagateFlow(startHeap: Heap.State): Boolean
}

object Heap {
    trait ValueHandle

    trait ValueHandleChangeHandler extends ValueChangeHandler {
        def onHandleChanged(oldValue: Option[Value], newValue: Value): Unit
        override def onValueChanged(): Unit = {}
    }

    trait ValueHandleMerger extends ValueHandle {
        def add(handles: ValueHandle*): Unit
    }

    trait HandleReader {
        def read(): Value
    }

    trait HandleWriter {
        def write(value: Value): Unit
    }

    trait State {
        def newHandleReader(handle: ValueHandle, handler: ValueHandleChangeHandler): (HandleReader, State)
        def newHandleWriter(handle: ValueHandle): (HandleWriter, State)
        def newValueHandleMerger(): (Heap.ValueHandleMerger, State)

        def truthyfy(cond: ValueHandle): State
        def falsyfy(cond: ValueHandle): State
    }

    trait MergeState extends State {
        def addInflow(heapState: State): Unit
    }
}