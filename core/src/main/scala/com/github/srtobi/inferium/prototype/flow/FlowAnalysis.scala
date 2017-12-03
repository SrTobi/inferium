package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast

import scala.collection.mutable



class FlowAnalysis private (val scriptTemplate: Templates.Script) {
    private val heap: Heap = ???
    private val unificationHeapState = heap.newHeapState()

    {
        val startHeap = heap.newHeapState()

        //instantiate(scriptTemplate, startHeap, unificationHeapState)
    }


    private def solveConstrains(): Boolean = {
        ???
    }

    private def analyseStep(): Boolean = {
        val changedNodes = heap.propagateFlow()

        changedNodes.foreach(_.inputChanged())

        return changedNodes.isEmpty
    }


    def analyse(): Unit = {
        var changed = false
        do {
            changed = analyseStep()
        } while (changed)
    }
}

object FlowAnalysis {
    def create(script: Ast.Script): FlowAnalysis = {
        val scriptTemplate = TemplateBuilder.buildScriptTemplate(script)
        return new FlowAnalysis(scriptTemplate)
    }
}