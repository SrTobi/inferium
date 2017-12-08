package com.github.srtobi.inferium.prototype.flow

trait FlowAnalysis {
    def solver: Solver
    def heap: Heap

    def activate(node: Nodes.Node, heapState: Heap.State): Unit
}
