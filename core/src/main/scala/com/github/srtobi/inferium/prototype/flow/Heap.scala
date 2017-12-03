package com.github.srtobi.inferium.prototype.flow

import Nodes.Node


trait ValueProvider {
    def usedBy(node: Node): Unit
    def notUsedBy(node: Node): Unit
    def fetch(): Value
    def asOption: Option[Value]
    def foreach(f: Value => _)
}

trait ValueSubmitter extends ValueProvider{
    def submit(value: Value): Unit
}

trait HeapValueChangeListener {
    def onHeapValueChange(): Unit
}

trait HeapReader {
    def read(target: Value): Value
}

trait HeapWriter {
    def write(target: Value, value: Value): Value
}

trait Heap {
    def newHeapState(): HeapState
    def addHeapFlow(from: HeapState, to: HeapState): Unit

    def newValueSubmitter(): ValueSubmitter
    def newHeapReader(state: HeapState, property: String): HeapReader
    def newHeapWriter(inState: HeapState, outState: HeapState, property: String): HeapWriter

    def propagateFlow(): Traversable[Node]

    def deriveHeapState(from: HeapState): HeapState = {
        val newState = newHeapState()
        addHeapFlow(from, newState)
        return newState
    }
}

trait HeapState {

}