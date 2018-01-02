package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast

object Templates {
    trait Script {
        def instantiate(flowAnalysis: FlowAnalysis, endNode: Nodes.Node): (Nodes.Node, Seq[HandleSourceProvider])
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

        def instantiate(closures: Seq[HeapHandle], arguments: Seq[HandleSourceProvider], endNode: Nodes.Node): (Nodes.Node, Seq[HandleSourceProvider])
    }
}
