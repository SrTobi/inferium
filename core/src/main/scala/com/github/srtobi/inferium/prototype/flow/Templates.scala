package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast

object Templates {
    trait Script {
        def instantiate(flowAnalysis: FlowAnalysis, endNode: Nodes.Node, returnMerger: Heap.ValueHandleMerger): Nodes.Node
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

        def instantiate(closures: Seq[Heap.ValueHandle], arguments: Seq[Heap.ValueHandle], endNode: Nodes.Node, returnMerger: Heap.ValueHandleMerger): Nodes.Node
    }
}
