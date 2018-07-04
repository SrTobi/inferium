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

        override def info(node: Node, message: String): Unit = println(message)

        override def hasError: Boolean = ???
    }

    def main(args: Array[String]): Unit = {
        val code =
            """
              |var lastObj
              |var someObj
              |while (debug.boolean) {
              |     debug(lastObj).print("l")
              |    lastObj = { prop: "init" }
              |    debug(someObj).print("some-before")
              |    if (debug.boolean) {
              |        someObj = lastObj
              |    }
              |
              |    debug(someObj).print("some")
              |    debug(someObj.prop).print("some.prop")
              |
              |    debug(lastObj.prop).is("init")
              |    lastObj.prop = "next"
              |    debug(lastObj.prop).is("next")
              |
              |
              |    if (debug.boolean) {
              |        someObj.prop = "blub"
              |    }
              |    debug(lastObj).print("end")
              |}
              |
              |debug(lastObj.prop).is("next", "blub")
              |debug(someObj.prop).is("next", "blub")
              |
              |
            """.stripMargin

        /*val code =
            """
              |var o = {}
              |o.o = o
              |
              |while(debug.boolean) {
              |  o = o.o
              |}
              |
            """.stripMargin*/

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val config = InferiumConfig.Env.NodeDebug
        val graph = new GraphBuilder(config).buildTemplate(prog).instantiate()

        //println(PrintVisitor.print(graph, printMergeNodes = true, showStackInfo = true))
        val analysis = new DataFlowAnalysis(graph, new TestDebugAdapter)

        analysis.runAnalysis(NodeJs.initialState(config))

        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))

        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))
    }

}
