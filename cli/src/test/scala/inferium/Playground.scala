package inferium

import escalima.ECMAScript
import inferium.dataflow.{DataFlowAnalysis, ExecutionState, GraphBuilder, LexicalFrame}
import inferium.dataflow.graph.visitors.{DotPrintVisitor, PrintVisitor, StackAnnotationVisitor}
import inferium.lattice.{Location, ObjectEntity, UndefinedValue}
import inferium.lattice.heaps.SimpleHeap

object Playground {
    def main(args: Array[String]): Unit = {

        val code =
            """
              |var a = 4
              |a
            """.stripMargin

        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val graph = new GraphBuilder(Config()).buildTemplate(prog).instantiate()

        val analysis = new DataFlowAnalysis(graph)

        val globalObj = ObjectEntity.ordinary(Location())
        val heap = {
            val initialHeap = new SimpleHeap()
            val mutator = initialHeap.begin(Location())
            mutator.allocObject(globalObj)
            initialHeap.end(mutator)
        }


        val iniState = new ExecutionState(UndefinedValue :: Nil, heap, LexicalFrame(globalObj))
        analysis.runAnalysis(iniState)

        println(new PrintVisitor(showStackInfo = true).start(graph))
        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))
    }
}
