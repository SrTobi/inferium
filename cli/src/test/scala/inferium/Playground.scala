package inferium

import escalima.ECMAScript
import inferium.dataflow.GraphBuilder
import inferium.dataflow.graph.visitors.{PrintVisitor, StackAnnotationVisitor}

object Playground {
    def main(args: Array[String]): Unit = {
        
        val code =
            """
              |var a = 5
              |{
              |const a = 9
              |}
            """.stripMargin

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val graph = new GraphBuilder(Config()).buildGraph(prog)

        new StackAnnotationVisitor().start(graph)
        println(new PrintVisitor(showStackInfo = false).start(graph))
    }
}
