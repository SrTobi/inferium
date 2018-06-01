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
        val code =
            """
              |function test(a) {
              |  if (debug.boolean) {
              |     test(9)
              |     return "haha"
              |  } else {
              |     return
              |  }
              |}
              |
              |let r = test("test")
              |debug(r).print()
            """.stripMargin

        /*val code =

              |if (false) {
              |  debug.liveCode()
              |}
            """.stripMargin*/

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
