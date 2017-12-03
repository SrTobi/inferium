package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast

object Templates {
    trait Script {
        def instantiate(solver: Solver, heap: Heap, inHeap: HeapState, outHeap: HeapState): Nodes.Function
    }

    trait Closure {
        def outer: Option[Closure]
        val closureIndex: Int

        def hasVar(name: String): Boolean
        def closureIndexForVar(name: String): Int
    }

    trait Function {
        def closure: Closure
        def parameter: Seq[String]

        def instantiate(closures: Seq[ValueProvider], inHeap: HeapState, outHeap: HeapState): Nodes.Function
    }
}
