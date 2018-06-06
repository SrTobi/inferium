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

        override def info(node: Node, message: String): Unit = println(message)

        override def hasError: Boolean = ???
    }

    def main(args: Array[String]): Unit = {
        /*val code =
            """
              |var lastObj
              |var someObj
              |while (debug.boolean) {
              |    lastObj = { prop: "init" }
              |    if (debug.boolean) {
              |        someObj = lastObj
              |    }
              |    debug(lastObj).print("obj")
              |    debug(lastObj.prop).is("init").print("here")
              |    lastObj.prop = "next"
              |    debug(lastObj.prop).is("next")
              |
              |
              |    if (debug.boolean) {
              |        someObj.prop = "blub"
              |    }
              |}
              |
              |debug(lastObj.prop).is("next", "blub")
              |debug(someObj.prop).is("next", "blub")
              |
              |someObj.absProp = "abs"
              |debug(someObj.absProp).is("abs", undefined)
            """.stripMargin*/

        val code =
            """
              |var a = false
              |function f(b) {
              | if (a) {
              |   return "blub"
              | }
              | if (b) {
              |     a = true
              |     return f(false)
              | } else {
              |     return "test"
              | }
              |}
              |let r = f(true)
              |debug(r).print()
              |
            """.stripMargin

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val graph = new GraphBuilder(InferiumConfig.Env.NodeDebug).buildTemplate(prog).instantiate()

        println(PrintVisitor.print(graph, printMergeNodes = true))
        val analysis = new DataFlowAnalysis(graph, new TestDebugAdapter)

        analysis.runAnalysis(NodeJs.initialState)

        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))

        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))
    }

}
