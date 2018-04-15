package inferium

import escalima.ECMAScript
import inferium.dataflow._
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.visitors.PrintVisitor
import inferium.prelude.NodeJs

object Playground {

    class TestDebugAdapter extends DebugAdapter {
        override def error(node: Node, message: String): Unit = println("Error: " + message)

        override def warn(node: Node, message: String): Unit = println("Warn: " + message)

        override def hasError: Boolean = ???
    }

    def main(args: Array[String]): Unit = {

        val code =
            """
              |/*
              |    name: lexical scopes
              |    desc: Lexical read and write should respect scopes
              | */
              |
              |var a = "a"
              |a
              |debug.ans.isOneOf("a")
              |
              |var b = "b"
              |
              |debug(a).isOneOf("a")
              |debug(b).isOneOf("b")
              |
              |debug(c).isOneOf(undefined)
              |var c = "c"
              |debug(c).isOneOf("c")
            """.stripMargin

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val graph = new GraphBuilder(InferiumConfig.Env.NodeDebug).buildTemplate(prog).instantiate()

        val analysis = new DataFlowAnalysis(graph, new TestDebugAdapter)
        analysis.runAnalysis(NodeJs.initialState)

        println(new PrintVisitor(showStackInfo = true).start(graph))
        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))
    }
}
