package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast

object Templates {
    trait Script {
        def instantiate(solver: Solver, heap: Heap, endNode: Nodes.Node): (Nodes.Node, ValueHandleMerger)
    }

    trait Closure {
        def outer: Option[Closure]
        val closureIndex: Int

        def hasVar(name: String): Boolean
        def closureIndexForVar(name: String): Int
    }

    trait Function {
        def closure: Closure
        def parameters: Seq[String]

        def instantiate(closures: Seq[Value], arguments: Seq[ValueHandle], endNode: Nodes.Node, returnMerger: ValueHandleMerger): Nodes.Node
    }
}
