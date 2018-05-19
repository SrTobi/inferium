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
              |var t = { cond: true }
              |var f = { cond: false }
              |var b = { cond: debug.boolean }
              |
              |if (debug.boolean) {
              |    var test = t
              |} else {
              |    test = f
              |}
              |
              |debug(test).isOneOf(t, f).print()
              |debug(test.cond).isOneOf(debug.boolean).print()
              |
              |if (test.cond) {
              |    debug(test).isOneOf(t)
              |    debug(t.cond).isOneOf(true).print()
              |    debug(f.cond).isOneOf(false).print()
              |} else {
              |    debug(test).isOneOf(f)
              |    debug(t.cond).isOneOf(true).print()
              |    debug(f.cond).isOneOf(false).print()
              |}
              |
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
