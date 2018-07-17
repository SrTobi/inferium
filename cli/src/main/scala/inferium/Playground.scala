package inferium

import escalima.ECMAScript
import inferium.dataflow._
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.visitors.PrintVisitor
import inferium.lattice.heaps.ChainHeap
import inferium.prelude.NodeJs

import scala.util.Random


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
              |exports.func = function(proc) {
              |  return (function(x) {
              |    return proc(function(y) { return (x(x))(y);});
              |  })(function(x) {
              |    return proc(function(y) { return (x(x))(y);});
              |  });
              |};
              |
            """.stripMargin

        /*val code =
            """
              |
            """.stripMargin*/

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val config = InferiumConfig.Env.NodeDebug
        val graph = new GraphBuilder(config).buildTemplate(prog, hasModule = true).instantiate()

        val (initialHeap, globalObject) = NodeJs.initialHeap(config, ChainHeap)
        //println(PrintVisitor.print(graph, printMergeNodes = true, showStackInfo = true))
        val analysis = new NodeModuleAnalysis(graph, globalObject, new TestDebugAdapter)
        //val analysis = new ScriptAnalysis(graph, new TestDebugAdapter)

        analysis.runAnalysis(initialHeap)
        //analysis.runAnalysis(NodeJs.initialState(config))

        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))

        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))


    }

}
