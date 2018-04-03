package inferium.dataflow

import inferium.dataflow.graph.Node
import inferium.lattice.Entity

object Templates {
    trait Script {
        def instantiate(): graph.ScriptGraph
    }

    /*trait Closure {
        def outer: Option[Closure]
        val closureIndex: Int

        def hasVar(name: String): Boolean
        def closureIndexForVar(name: String): Int
    }*/


    //type CallStack = Map[Function, Nodes.FunctionCall]
    trait Function {

        //def closure: Closure
        def parameters: Seq[String]

        def instantiate(): (Node, Node)
    }
}
