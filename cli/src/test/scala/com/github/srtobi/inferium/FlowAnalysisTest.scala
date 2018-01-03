package com.github.srtobi.inferium

import com.github.srtobi.inferium.prototype.{Ast, LangParser}
import com.github.srtobi.inferium.prototype.flow._
import fastparse.core.Parsed


object FlowAnalysisTest {
    import ForwardFlowAnalysis.IniObject
    private def analyse(script: Ast.Script): Unit = {

        val global = IniObject("rand" -> BoolValue)

        val analysis = ForwardFlowAnalysis.create(script, Solver, new IterationHeap, global)
        analysis.analyse()
    }
    def main(args: Array[String]): Unit = {

        val code =
            """
              |if (rand) {
              | return 5 - 9
              |} else {
              | return true
              |}
            """.stripMargin

        LangParser.script.parse(code) match {
            case Parsed.Success(script, _) =>
                analyse(script)
            case f@Parsed.Failure(lastParser, _, extra) =>
                println(f)
        }
    }
}