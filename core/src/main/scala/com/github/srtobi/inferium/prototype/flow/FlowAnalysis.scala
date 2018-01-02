package com.github.srtobi.inferium.prototype.flow

trait FlowAnalysis {
    def solver: Solver

    def controlFlowTo(node: Nodes.Node, heapState: HeapMemory): Unit
    def noControlFlowTo(node: Nodes.Node): Unit

    def unify(heaps: HeapMemory*): HeapMemory
}
