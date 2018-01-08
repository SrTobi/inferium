package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap.{IniEntity, IniObject}
import com.github.srtobi.inferium.prototype.{Ast, LangParser}
import fastparse.core.Parsed

import scala.collection.mutable


class ForwardFlowAnalysis private(val scriptTemplate: Templates.Script, override val solver: Solver, val heapImpl: Heap, global: IniObject) extends FlowAnalysis {

    import Nodes.Node

    private var _scriptReturnValue: Value = _
    private var globalHeapState = heapImpl.newEmptyHeapState()
    private val globalObject = Heap.writeIniObjectToHeap(globalHeapState, global)
    private val nodesToPropagate = mutable.Queue.empty[(Node, Option[HeapMemory])]
    private val knownFunctions = mutable.Set.empty[FunctionValue]
    private val knownObjects = mutable.Set.empty[ObjectValue]


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

    private def garbageCollect(heap: HeapMemory, returnValue: ValueLike): HeapMemory = {
        val result = heapImpl.newEmptyHeapState()
        val objects = mutable.Set.empty[ObjectValue]
        val queue = mutable.Queue.empty[ObjectValue]

        def add(value: Value): Unit = value match {
            case union: UnionValue => union.baseObjects.foreach(queue.enqueue(_))
            case obj: ObjectValue =>
                obj match {
                    case func: FunctionValue =>
                        knownFunctions += func
                        func.closures.map(_.asValue).foreach(add)
                    case _ =>
                }
                knownObjects += obj
                queue.enqueue(obj)
            case _ =>
        }

        queue += globalObject
        queue ++= knownObjects
        add(returnValue.normalized)

        while (queue.nonEmpty) {
            val obj = queue.dequeue()

            if (!objects.contains(obj)) {
                objects += obj
                for (prop <- heap.listProperties(obj).iterator) {
                    val value = heap.readProperty(obj, prop, cache = false).normalized
                    result.writeProperty(obj, prop, value)

                    add(value)
                }
            }
        }

        return result
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

        _scriptReturnValue = UnionValue(returns.map(_.newSource().get()): _*).normalized

        globalHeapState = resultingHeap.map(garbageCollect(_, _scriptReturnValue)).getOrElse(globalHeapState)
    }


    def globalHeap: HeapMemory = globalHeapState
    def scriptReturnValue: ValueLike = _scriptReturnValue
    def scriptReturn: IniEntity = globalHeapState.toIniEntity(Seq(scriptReturnValue)).head._2

    def analyse(): Unit = {
        analyseInitialScriptExecution()
    }

    override def unify(heaps: HeapMemory*): HeapMemory = heapImpl.unify(heaps: _*)
}

object ForwardFlowAnalysis {

    def create(script: Ast.Script, solver: Solver, heapImpl: Heap, global: IniObject): ForwardFlowAnalysis = {
        val scriptTemplate = TemplateBuilder.buildScriptTemplate(script)
        return new ForwardFlowAnalysis(scriptTemplate, solver, heapImpl, global)
    }
}
