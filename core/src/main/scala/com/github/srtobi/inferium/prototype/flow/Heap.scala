package com.github.srtobi.inferium.prototype.flow


//abstract class HeapHandle

abstract class HeapMemory {
    //def read(handle: HeapHandle): ValueLike
    //def write(handle: HeapHandle, value: ValueLike): Unit
    def readProperty(target: ValueLike, propertyName: String): ValueLike
    def writeProperty(target: ValueLike, propertyName: String, handle: ValueLike): Unit

    def split(): HeapMemory
}

abstract class Heap {
    def newEmptyHeapState(): HeapMemory

    def unify(heaps: HeapMemory*): HeapMemory
}



/*
import Nodes.Node

class HeapStateBuilder(private var cur: HeapState.State, private val node: Node) {
    def newHandleReader(handle: HeapState.ValueHandle, handler: HeapState.ValueHandleChangeHandler): HeapState.HandleReader = {
        assert(isBuilding)
        val (reader, newHeap) = cur.newHandleReader(handle, handler)
        cur = newHeap
        return reader
    }

    def newHandleWriter(handle: HeapState.ValueHandle): HeapState.HandleWriter = {
        assert(isBuilding)
        val (writer, newHeap) = cur.newHandleWriter(handle)
        cur = newHeap
        return writer
    }

    def newHandleMerge(): HeapState.ValueHandleMerger = {
        assert(isBuilding)
        val (merger, newHeap) = cur.newValueHandleMerger()
        cur = newHeap
        return merger
    }

    /*def readProperty(target: HeapState.ValueHandle, property: String, into: HeapState.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.readProperty(target, property, into)
    }

    def writeProperty(target: HeapState.ValueHandle, property: String, value: HeapState.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.writeProperty(target, property, value)
    }

    def readLocal(target: EmptyObject, property: String, into: HeapState.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.readLocal(target, property, into)
    }

    def writeLocal(target: EmptyObject, property: String, value: HeapState.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.writeLocal(target, property, value)
    }*/

    def end(): HeapState.State = {
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

trait HeapState {
    def newEmptyHeapState(): HeapState.State
    def newMergeHeapState(numTip: Int = 0): HeapState.MergeState
    def newValueHandle(): HeapState.ValueHandle

    def propagateFlow(startHeap: HeapState.State): Boolean
}

object HeapState {
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
        def newValueHandleMerger(): (HeapState.ValueHandleMerger, State)

        def truthyfy(cond: ValueHandle): State
        def falsyfy(cond: ValueHandle): State
    }

    trait MergeState extends State {
        def addInflow(heapState: State): Unit
    }
}*/