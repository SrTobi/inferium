package com.github.srtobi.inferium

import com.github.srtobi.inferium.prototype.{Ast, LangParser}
import com.github.srtobi.inferium.prototype.flow._
import fastparse.core.Parsed


object Playground {
    import Heap.IniObject
    private def analyse(script: Ast.Script): Unit = {

        val global = IniObject("rand" -> BoolValue)

        val analysis = ForwardFlowAnalysis.create(script, Solver, new IterationHeap, global)
        analysis.analyse()
        val result = analysis.scriptReturn

        val printer = new TypeScriptPrinter(result, null)
        println(printer.print())
    }

    def main(args: Array[String]): Unit = {

        val ycode =
            """
              |var Y = (f) => {
              |    var y = (newP) => {
              |        return Y(f)(newP)
              |    }
              |    return (p) => { return f(y, p) }
              |}
              |
              |return Y
            """.stripMargin

        val code =
            """
              |
              |var fold = (folder, it, init) => {
              |    if (it.hasNext()) {
              |      return fold(folder, it, folder(init, it.next()))
              |    } else {
              |      return init
              |    }
              |}
              |
              |return fold
              |
            """.stripMargin

        LangParser.script.parse(code) match {
            case Parsed.Success(script, _) =>
                analyse(script)
            case f@Parsed.Failure(lastParser, _, extra) =>
                println(f)
        }
    }
}