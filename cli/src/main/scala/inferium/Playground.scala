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
              |let a = [1, 2,,, "test"]
              |debug(a[1]).is(2)
              |debug(a[4]).is("test")
              |debug(a.length).is(5)
              |
              |debug(a[debug.number]).is(undefined, debug.number, "test")
            """.stripMargin

        /*val code =
            """
              |
            """.stripMargin*/

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val config = InferiumConfig.Env.NodeDebug
        val graph = new GraphBuilder(config).buildTemplate(prog).instantiate()

        //println(PrintVisitor.print(graph, printMergeNodes = true, showStackInfo = true))
        val analysis = new ScriptAnalysis(graph, new TestDebugAdapter)

        analysis.runAnalysis(NodeJs.initialState(config))

        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))

        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))
    }

}
