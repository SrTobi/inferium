package com.github.srtobi.inferium.web

import java.text.ParseException

import com.github.srtobi.inferium.prototype.LangParser
import com.github.srtobi.inferium.prototype.flow.Heap.IniObject
import com.github.srtobi.inferium.prototype.flow._
import fastparse.core.Parsed

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


@JSExportTopLevel("Inferium")
object Api {
    @JSExport
    def generateTypeDefinition(code: String): String = {

        LangParser.script.parse(code) match {
            case Parsed.Success(script, _) =>
              val global = IniObject("rand" -> BoolValue)

              val analysis = ForwardFlowAnalysis.create(script, Solver, new IterationHeap, global)
              analysis.analyse()
              val result = analysis.scriptReturn

              val printer = new TypeScriptPrinter(result, null)
              printer.print()
            case f@Parsed.Failure(lastParser, _, extra) =>
              throw js.JavaScriptException(f.toString())
        }
    }
}