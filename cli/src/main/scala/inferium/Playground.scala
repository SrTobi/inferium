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
              |function test(x) {
              |  debug(x).print()
              |  debug(this).print()
              |  this.prop = x
              |}
              |
              |if (debug.boolean) {
              |  test.prototype = { p: "hihi" }
              |} else {
              |  test.prototype = { p: "haha" }
              |}
              |
              |var o = new test("xxx")
              |debug(o).print("o")
              |debug(o.prop).print("o.prop")
              |debug(o.p).print("o.p")
              |
            """.stripMargin

        /*val code =
            """
              |var o = {}
              |
              |if (debug.boolean) {
              |  o.p = "test"
              |}
              |
              |debug(o.p).print("o.p")
              |
            """.stripMargin*/

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val config = InferiumConfig.Env.NodeDebug
        val graph = new GraphBuilder(config).buildTemplate(prog).instantiate()

        println(PrintVisitor.print(graph, printMergeNodes = true, showStackInfo = true))
        val analysis = new DataFlowAnalysis(graph, new TestDebugAdapter)

        analysis.runAnalysis(NodeJs.initialState(config))

        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))

        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))
    }

}
