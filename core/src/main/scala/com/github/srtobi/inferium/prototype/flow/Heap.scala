package com.github.srtobi.inferium.prototype.flow

import Nodes.Node


trait ValueHandle
/*
trait ValueProvider {
    def usedBy(node: Node): Unit
    def notUsedBy(node: Node): Unit
    def fetch(): Value
    def asOption: Option[Value]
    def foreach(f: Value => Unit)
}

trait ValueSubmitter extends ValueProvider{
    def submit(value: Value): Unit
}*/

trait ValueHandleMerger extends ValueHandle {
    def add(handles: ValueHandle*): Unit
}

trait HandleReader {
    def read(): Value
}

trait HandleWriter {
    def write(value: Value): Unit
}

trait HeapState {
    def newHandleReader(handle: ValueHandle, node: Node): (HandleReader, HeapState)
    def newHandleWriter(handle: ValueHandle, node: Node): (HandleWriter, HeapState)
    def readProperty(target: ValueHandle, property: String, into: ValueHandle): HeapState
    def writeProperty(target: ValueHandle, property: String, value: ValueHandle): HeapState
    def readLocal(target: EmptyObject, name: String, into: ValueHandle): HeapState
    def writeLocal(target: EmptyObject, name: String, value: ValueHandle): HeapState
    def truthyfy(cond: ValueHandle): HeapState
    def falsyfy(cond: ValueHandle): HeapState
}

trait MergeHeapState extends HeapState {
    def addInflow(heapState: HeapState): Unit
}

class HeapStateBuilder(private var cur: HeapState, private val node: Node) {
    def newHandleReader(handle: ValueHandle): HandleReader = {
        assert(isBuilding)
        val (reader, newHeap) = cur.newHandleReader(handle, node)
        cur = newHeap
        return reader
    }

    def newHandleWriter(handle: ValueHandle): HandleWriter = {
        assert(isBuilding)
        val (writer, newHeap) = cur.newHandleWriter(handle, node)
        cur = newHeap
        return writer
    }

    def readProperty(target: ValueHandle, property: String, into: ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.readProperty(target, property, into)
    }

    def writeProperty(target: ValueHandle, property: String, value: ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.writeProperty(target, property, value)
    }

    def readLocal(target: EmptyObject, property: String, into: ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.readLocal(target, property, into)
    }

    def writeLocal(target: EmptyObject, property: String, value: ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.writeLocal(target, property, value)
    }

    def end(): HeapState = {
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
    def newEmptyHeapState(): HeapState
    def newMergeHeapState(): MergeHeapState
    def newValueHandle(): ValueHandle
    def newValueHandleMerger(): ValueHandleMerger

    def propagateFlow(): Boolean
}

object Heap {

}