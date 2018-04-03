package inferium

import escalima.ECMAScript
import inferium.dataflow.{DataFlowAnalysis, ExecutionState, GraphBuilder, LexicalFrame}
import inferium.dataflow.graph.visitors.{PrintVisitor, StackAnnotationVisitor}
import inferium.lattice.{Location, ObjLocation, UndefinedValue}
import inferium.lattice.heaps.SimpleHeap

object Playground {
    def main(args: Array[String]): Unit = {
        
        val code =
            """
              |if (4) {
              |  8
              |} else {
              |  4
              |}
            """.stripMargin

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val graph = new GraphBuilder(Config()).buildTemplate(prog).instantiate()

        val analysis = new DataFlowAnalysis(graph)

        val globalObj = ObjLocation(Location())
        val iniState = new ExecutionState(UndefinedValue :: Nil, new SimpleHeap(), LexicalFrame(globalObj))
        analysis.runAnalysis(iniState)

        println(new PrintVisitor(showStackInfo = true).start(graph))
    }
}
