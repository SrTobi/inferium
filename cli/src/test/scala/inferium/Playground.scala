package inferium

import escalima.ECMAScript
import inferium.dataflow.GraphBuilder
import inferium.dataflow.graph.visitors.{PrintVisitor, StackAnnotationVisitor}

object Playground {
    def main(args: Array[String]): Unit = {

        val code =
            """
              |if(4) {
              | 5
              |} else {
              | 4
              |}
            """.stripMargin

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val graph = GraphBuilder.buildGraph(prog)

        new StackAnnotationVisitor().start(graph)
        println(new PrintVisitor(showStackInfo = true).start(graph))
    }
}
