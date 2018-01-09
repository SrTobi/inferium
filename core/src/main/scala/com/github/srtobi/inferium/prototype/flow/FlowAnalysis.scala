package com.github.srtobi.inferium.prototype.flow

trait FlowAnalysis {
    trait FunctionInfo {
        def function: FunctionValue
        def returnValue: Value
        def arguments: Seq[UserValue]
    }

    def solver: Solver

    def controlFlowTo(node: Nodes.Node, heapState: HeapMemory): Unit
    def noControlFlowTo(node: Nodes.Node): Unit

    def getFunctionInfo(function: FunctionValue): Option[FunctionInfo]

    def unify(heaps: HeapMemory*): HeapMemory
}
