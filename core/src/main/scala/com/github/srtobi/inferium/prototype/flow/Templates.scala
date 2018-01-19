package com.github.srtobi.inferium.prototype.flow

object Templates {
    trait Script {
        def instantiate(flowAnalysis: FlowAnalysis, global: ObjectValue, endNode: Nodes.Node): (Nodes.Node, Seq[ValueSourceProvider])
    }

    trait Closure {
        def outer: Option[Closure]
        val closureIndex: Int

        def hasVar(name: String): Boolean
        def closureIndexForVar(name: String): Int
    }


    type CallStack = Map[Function, Nodes.FunctionCall]
    trait Function {

        def closure: Closure
        def parameters: Seq[String]

        def instantiate(closures: Seq[ValueLike], arguments: Seq[ValueSourceProvider], callstack: CallStack, endNode: Nodes.Node): (Nodes.Node, Seq[ValueSourceProvider])
    }
}
