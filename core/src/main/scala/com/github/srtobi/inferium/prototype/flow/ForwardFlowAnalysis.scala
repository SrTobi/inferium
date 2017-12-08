package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast

import scala.collection.mutable



class ForwardFlowAnalysis private(val scriptTemplate: Templates.Script, override val solver: Solver, override val heap: Heap) extends FlowAnalysis {

    import Nodes.Node

    private val endNode = new Node()(this)
    private val nodesToActivate = mutable.Queue.empty[(Node, Heap.State)]
    private val (beginNode, unificationHeapState) = scriptTemplate.instantiate(this, endNode)



    /*def analyse(): Unit = {
        var changed = false
        do {
            changed = analyseStep()
        } while (changed)
    }*/
    override def activate(node: Node, heapState: Heap.State): Unit = {
        nodesToActivate.enqueue((node, heapState))
    }

    private def activateNodes(): Boolean = {
        val changed = nodesToActivate.nonEmpty
        while (nodesToActivate.nonEmpty) {
            val (node, heapState) = nodesToActivate.dequeue()
            node.doActivation(heapState)
        }
        return changed
    }

    private def analyseFlowStep(): Boolean = {
        var changed = false

        changed = activateNodes()
        changed = heap.propagateFlow() || changed

        return changed
    }

    private def analyseFlow(): Boolean = {
        var changed = false

        while(analyseFlowStep()) {
            changed = true
        }

        return changed
    }

    private def analyse(): Unit = {
        analyseFlow()
    }
}

object ForwardFlowAnalysis {
    def create(script: Ast.Script, solver: Solver, heap: Heap): ForwardFlowAnalysis = {
        val scriptTemplate = TemplateBuilder.buildScriptTemplate(script)
        return new ForwardFlowAnalysis(scriptTemplate, solver, heap)
    }
}