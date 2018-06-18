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
            """/*
              |    name: for flow
              |    desc: for-loops should handle init-, test- and update-expressions correctly
              | */
              |
              |
              |for (var a = "init"; debug.boolean; a = "update") {
              |    debug(a).is("init", "update")
              |    a = "inner"
              |    debug(a).is("inner")
              |}
              |
              |debug(a).is("init", "update")
              |debug("1!").print()
              |
              |let b = "outer"
              |
              |for (let b = "init"; debug.boolean; debug(b).is("inner1")) {
              |    debug(b).is("init", "inner1")
              |    b = "inner1"
              |    let b = "inner2"
              |    debug(b).is("inner2")
              |}
              |
              |debug(b).is("outer")
              |debug("2!").print()
              |
              |
              |var c = false
              |
              |for (c = true; c; c = false) {
              |    debug(c).is(true)
              |}
              |debug("3!").print()
              |
              |debug(c).is(false)
              |
              |for (; debug.boolean;) {
              |    debug.liveCode()
              |}
              |
              |debug("4!").print()
              |debug.liveCode()
              |
              |for (;;) {
              |}
              |
              |debug.deadCode()
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

        val graph = new GraphBuilder(InferiumConfig.Env.NodeDebug).buildTemplate(prog).instantiate()

        println(PrintVisitor.print(graph, printMergeNodes = true))
        val analysis = new DataFlowAnalysis(graph, new TestDebugAdapter)

        analysis.runAnalysis(NodeJs.initialState)

        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))

        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))
    }

}
