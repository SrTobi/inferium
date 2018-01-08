package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap.{IniEntity, IniObject}
import com.github.srtobi.inferium.prototype.{Ast, LangParser}
import fastparse.core.Parsed

import scala.collection.mutable


class ForwardFlowAnalysis private(val scriptTemplate: Templates.Script, override val solver: Solver, val heap: Heap, global: IniObject) extends FlowAnalysis {

    import Nodes.Node

    private var _scriptReturnValue: Value = _
    private var globalHeapState = heap.newEmptyHeapState()
    private val globalObject = Heap.writeIniObjectToHeap(globalHeapState, global)
    private val nodesToPropagate = mutable.Queue.empty[(Node, Option[HeapMemory])]


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

    private def analyseInitialScriptExecution(): Unit = {
        // analyse initial code
        assert(nodesToPropagate.isEmpty)
        propagateControlFlow()
        var resultingHeap: Option[HeapMemory] = None

        val endNode = new Node()(this) {
            override def onControlFlow(heap: HeapMemory): Unit = {
                resultingHeap = Some(heap)
                assert(nodesToPropagate.isEmpty)
            }
            override def onNoControlFlow(): Unit = {
                assert(nodesToPropagate.isEmpty)
            }
        }

        val mergeNode = new Nodes.MergeNode(0, endNode)(this)
        val (beginNode, returns) = scriptTemplate.instantiate(this, globalObject, mergeNode)

        mergeNode.setNumBranchesToWaitFor(returns.length)
        controlFlowTo(beginNode, globalHeapState)

        // analyse
        analyseFlow()

        globalHeapState = resultingHeap.getOrElse(globalHeapState)
        _scriptReturnValue = UnionValue(returns.map(_.newSource().get()): _*).normalized
    }

    def globalHeap: HeapMemory = globalHeapState
    def scriptReturnValue: ValueLike = _scriptReturnValue
    def scriptReturn: IniEntity = globalHeapState.toIniEntity(Seq(scriptReturnValue)).head._2

    def analyse(): Unit = {
        analyseInitialScriptExecution()
    }

    override def unify(heaps: HeapMemory*): HeapMemory = heap.unify(heaps: _*)
}

object ForwardFlowAnalysis {

    def create(script: Ast.Script, solver: Solver, heap: Heap, global: IniObject): ForwardFlowAnalysis = {
        val scriptTemplate = TemplateBuilder.buildScriptTemplate(script)
        return new ForwardFlowAnalysis(scriptTemplate, solver, heap, global)
    }
}
