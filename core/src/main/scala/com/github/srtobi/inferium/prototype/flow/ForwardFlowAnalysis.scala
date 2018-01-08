package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap.IniObject
import com.github.srtobi.inferium.prototype.{Ast, LangParser}
import fastparse.core.Parsed

import scala.collection.mutable


class ForwardFlowAnalysis private(val scriptTemplate: Templates.Script, override val solver: Solver, val heap: Heap, global: IniObject) extends FlowAnalysis {

    import Nodes.Node

    private val endNode = new Node()(this) {
        override def onControlFlow(newHeap: HeapMemory): Unit = {
            globalHeapState = heap.unify(globalHeapState.split(), newHeap.split())
            assert(nodesToPropagate.isEmpty)
        }
        override def onNoControlFlow(): Unit = {
            assert(nodesToPropagate.isEmpty)
        }
    }
    private var globalHeapState = heap.newEmptyHeapState()
    private val globalObject = Heap.writeIniObjectToHeap(globalHeapState, global)
    private val mergeNode = new Nodes.MergeNode(0, endNode)(this)
    private val nodesToPropagate = mutable.Queue.empty[(Node, Option[HeapMemory])]
    private val (beginNode, returns) = scriptTemplate.instantiate(this, globalObject, mergeNode)

    mergeNode.setNumBranchesToWaitFor(returns.length)
    controlFlowTo(beginNode, globalHeapState)


    override def controlFlowTo(node: Nodes.Node, heapState: HeapMemory): Unit = {
        assert(node ne null)
        assert(heapState ne null)
        nodesToPropagate.enqueue((node, Some(heapState)))
    }

    override def noControlFlowTo(node: Nodes.Node): Unit = {
        assert(node ne null)
        nodesToPropagate.enqueue((node, None))
    }

    private def propagateControlFlow(): Unit = {
        val changed = nodesToPropagate.nonEmpty
        while (nodesToPropagate.nonEmpty) {
            val (node, heapState) = nodesToPropagate.dequeue()
            heapState match {
                case Some(state) => node.onControlFlow(state)
                case None => node.onNoControlFlow()
            }
        }
    }

    private def analyseControlFlowStep(): Boolean = {
        var changed = false

        propagateControlFlow()

        return changed
    }

    private def analyseFlow(): Boolean = {
        var changed = false

        while(analyseControlFlowStep()) {
            changed = true
        }

        return changed
    }

    def globalHeap: HeapMemory = globalHeapState
    def scriptReturn: ValueLike = UnionValue(returns.map(_.newSource().get()): _*)

    def analyse(): Unit = {
        // analyse initial code
        assert(nodesToPropagate.nonEmpty)
        propagateControlFlow()
    }

    override def unify(heaps: HeapMemory*): HeapMemory = heap.unify(heaps: _*)
}

object ForwardFlowAnalysis {

    def create(script: Ast.Script, solver: Solver, heap: Heap, global: IniObject): ForwardFlowAnalysis = {
        val scriptTemplate = TemplateBuilder.buildScriptTemplate(script)
        return new ForwardFlowAnalysis(scriptTemplate, solver, heap, global)
    }
}
