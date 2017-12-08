package com.github.srtobi.inferium.prototype.flow

import Nodes.Node


class HeapStateBuilder(private var cur: Heap.State, private val node: Node) {
    def newHandleReader(handle: Heap.ValueHandle): Heap.HandleReader = {
        assert(isBuilding)
        val (reader, newHeap) = cur.newHandleReader(handle, node)
        cur = newHeap
        return reader
    }

    def newHandleWriter(handle: Heap.ValueHandle): Heap.HandleWriter = {
        assert(isBuilding)
        val (writer, newHeap) = cur.newHandleWriter(handle, node)
        cur = newHeap
        return writer
    }

    def readProperty(target: Heap.ValueHandle, property: String, into: Heap.ValueHandle): Unit = {
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
    }

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
    def newMergeHeapState(): Heap.MergeState
    def newValueHandle(): Heap.ValueHandle
    def newValueHandleMerger(): Heap.ValueHandleMerger

    def propagateFlow(): Boolean
}

object Heap {
    trait ValueHandle

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
        def newHandleReader(handle: ValueHandle, node: Node): (HandleReader, State)
        def newHandleWriter(handle: ValueHandle, node: Node): (HandleWriter, State)
        def readProperty(target: ValueHandle, property: String, into: ValueHandle): State
        def writeProperty(target: ValueHandle, property: String, value: ValueHandle): State
        def readLocal(target: EmptyObject, name: String, into: ValueHandle): State
        def writeLocal(target: EmptyObject, name: String, value: ValueHandle): State
        def truthyfy(cond: ValueHandle): State
        def falsyfy(cond: ValueHandle): State
    }

    trait MergeState extends State {
        def addInflow(heapState: State): Unit
    }
}