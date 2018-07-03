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
              |const x = debug.any
              |
              |debug(x).is(debug.any)
              |debug(x.x).is(debug.any)
              |debug(x.x.x).is(debug.any)
              |const a = x.x = 5
              |debug(a).is(5)
              |debug(x.x).is(debug.any)
              |
              |debug(x[debug.any]).is(debug.any)
              |debug(x[a]).is(debug.any)
            """.stripMargin

        /*val code =
            """
              |var a = false
              |function f(b) {
              | if (a) {
              |   return "blub"
              | }
              | if (b) {
              |     a = true
              |     return f(ffalse)
              | } else {
              |     return "test"
              | }
              |}
              |let r = f(true)
              |debug(r).print()
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
