package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast

import scala.collection.mutable



class FlowAnalysis private (val scriptTemplate: Templates.Script, private val solver: Solver, private val heap: Heap) {
    private val unificationHeapState = heap.newMergeHeapState()

    {
        val startHeap = heap.newEmptyHeapState()

        //instantiate(scriptTemplate, startHeap, unificationHeapState)
    }


    private def solveConstrains(): Boolean = {
        ???
    }

    private def analyseStep(): Boolean = {
        ???
    }


    def analyse(): Unit = {
        var changed = false
        do {
            changed = analyseStep()
        } while (changed)
    }
}

object FlowAnalysis {
    def create(script: Ast.Script, solver: Solver, heap: Heap): FlowAnalysis = {
        val scriptTemplate = TemplateBuilder.buildScriptTemplate(script)
        return new FlowAnalysis(scriptTemplate, solver, heap)
    }
}