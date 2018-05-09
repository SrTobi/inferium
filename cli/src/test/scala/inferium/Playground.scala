package inferium

import escalima.ECMAScript
import inferium.dataflow._
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.visitors.PrintVisitor
import inferium.prelude.NodeJs
import inferium.utils.macros.blockRec



object Playground {

    class TestDebugAdapter extends DebugAdapter {
        override def error(node: Node, message: String): Unit = println("Error: " + message)

        override def warn(node: Node, message: String): Unit = println("Warn: " + message)

        override def hasError: Boolean = ???
    }

    def main(args: Array[String]): Unit = {
        val code =
            """
              |var lastObj
              |//var someObj
              |while (debug.boolean) {
              |    lastObj = { prop: "init" }
              |    debug(lastObj).print()
              |    if (debug.boolean) {
              |        //someObj = lastObj
              |    }
              |
              |    debug(lastObj).print()
              |    debug(lastObj.prop).isOneOf("init").print()
              |
              |    /*if (debug.boolean) {
              |        someObj.prop = "blub"
              |    }*/
              |}
              |
              |//debug(lastObj.prop).isOneOf("init", "blub")
              |//debug(someObj.prop).isOneOf("init", "blub")
            """.stripMargin

        /*val code =
            """
              |var a = "test"
              |while(debug.boolean) {
              |  a = "1"
              |  while(debug.boolean) {
              |    a = "2"
              |    debug(a).print()
              |  }
              |  debug(a).print()
              |}
              |debug(a).print()
            """.stripMargin*/

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val graph = new GraphBuilder(InferiumConfig.Env.NodeDebug).buildTemplate(prog).instantiate()

        println(new PrintVisitor(showStackInfo = false).start(graph))
        val analysis = new DataFlowAnalysis(graph, new TestDebugAdapter)

        analysis.runAnalysis(NodeJs.initialState)

        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))
    }

}
