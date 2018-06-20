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
              |var o = { p: "init" }
              |
              |debug(o.p).is("init")
              |
              |o[debug.string] = "haha"
              |
              |debug(o.p).is("haha", "init")
              |debug(o[debug.string]).is("haha", "init", undefined)
              |
              |o.p = "reset"
              |
              |debug(o.p).is("reset")
              |debug(o[debug.string]).is("haha", "reset", undefined)
              |
              |
              |
              |var o2 = { [42]: "init", p: "string" }
              |
              |debug(o2.p).is("string")
              |debug(o2[42]).is("init")
              |debug(o2[debug.number]).is("init")
              |debug(o2[debug.string]).is("string", "init")
              |
              |o2[debug.number] = "haha"
              |
              |debug(o2.p).is("string")
              |debug(o2[42]).is("haha", "init")
              |debug(o2[debug.number]).is("haha", "init")
              |debug(o2[debug.string]).is("string", "haha", "init")
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
