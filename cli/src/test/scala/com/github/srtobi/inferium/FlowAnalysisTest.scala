package com.github.srtobi.inferium

import com.github.srtobi.inferium.prototype.{Ast, LangParser}
import com.github.srtobi.inferium.prototype.flow.{ForwardFlowAnalysis, IterationHeap, Solver}
import fastparse.core.Parsed


object FlowAnalysisTest {
    private def analyse(script: Ast.Script): Unit = {
        val analysis = ForwardFlowAnalysis.create(script, Solver, new IterationHeap)
        analysis.analyse()
    }
    def main(args: Array[String]): Unit = {

        val code =
            """
              |var x = {}
              |var y = x
              |x.xxx = "test"
              |return y.xxx.x
            """.stripMargin

        LangParser.script.parse(code) match {
            case Parsed.Success(script, _) =>
                analyse(script)
            case f@Parsed.Failure(lastParser, _, extra) =>
                println(f)
        }
    }
}