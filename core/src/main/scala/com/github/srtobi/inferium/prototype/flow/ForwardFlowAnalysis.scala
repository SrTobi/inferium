package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap.{IniEntity, IniObject}
import com.github.srtobi.inferium.prototype.{Ast, LangParser}
import fastparse.core.Parsed

import scala.collection.mutable


class ForwardFlowAnalysis private(val scriptTemplate: Templates.Script, override val solver: Solver, val heapImpl: Heap, global: IniObject) extends FlowAnalysis {

    import Nodes.Node

    private class EndNode extends Nodes.Node()(ForwardFlowAnalysis.this) {
        var resultingHeap: Option[HeapMemory] = None
        override def onControlFlow(heap: HeapMemory): Unit = {
            resultingHeap = Some(heap)
            assert(nodesToPropagate.isEmpty)
        }
        override def onNoControlFlow(): Unit = {
            assert(nodesToPropagate.isEmpty)
        }
    }

    private class CallContext(override val function: FunctionValue) extends FunctionInfo{
        val arguments: Seq[UserValue] = Seq.fill(function.template.parameters.length)(new UserValue)
        var returnValue: Value = NeverValue

        def fetchResultingHeap(): Option[HeapMemory] = {
            val res = endNode.resultingHeap
            endNode.resultingHeap = None
            return res
        }
        private val endNode = new EndNode
        val callNode = new Nodes.FunctionCall(ValueSource.wrap(function), arguments.map(ValueSink.wrap))(ForwardFlowAnalysis.this)
        val returnSource: ValueSource = callNode.result.newSource()
        callNode.next = endNode

    }

    private var _scriptReturnValue: Value = _
    private var globalHeapState = heapImpl.newEmptyHeapState()
    private val globalObject = Heap.writeIniObjectToHeap(globalHeapState, global)
    private val nodesToPropagate = mutable.Queue.empty[(Node, Option[HeapMemory])]
    private val knownFunctions = mutable.Map.empty[FunctionValue, CallContext]
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


    private def garbageCollect(oldHeap: HeapMemory, newHeap: HeapMemory, returnValue: ValueLike): (HeapMemory, Boolean) = {
        val result = heapImpl.newEmptyHeapState()
        val objects = mutable.Set.empty[ObjectValue]
        val queue = mutable.Queue.empty[ObjectValue]
        var changed = false

        def add(value: Value): Unit = value match {
            case union: UnionValue => union.baseObjects.foreach(add)
            case obj: ObjectValue =>
                obj match {
                    case func: FunctionValue =>
                        if (!knownFunctions.contains(func)) {
                            knownFunctions += (func -> new CallContext(func))
                        }
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
                for (prop <- newHeap.listProperties(obj).iterator) {
                    val oldValue = oldHeap.readProperty(obj, prop, cache = false)
                    assert(oldValue.isNormalized)
                    val value = newHeap.readProperty(obj, prop, cache = false).normalized
                    val resultValue = UnionValue(oldValue, value).normalized

                    if (!oldValue.structureEquals(resultValue)) {
                        changed = true
                    }

                    result.writeProperty(obj, prop, resultValue)

                    add(resultValue)
                }
            }
        }

        return (result, changed)
    }

    private def analyseInitialScriptExecution(): Unit = {
        // analyse initial code
        assert(nodesToPropagate.isEmpty)

        val endNode = new EndNode
        val mergeNode = new Nodes.MergeNode(0, endNode)(this)
        val (beginNode, returns) = scriptTemplate.instantiate(this, globalObject, mergeNode)

        mergeNode.setNumBranchesToWaitFor(returns.length)
        controlFlowTo(beginNode, globalHeapState)

        // analyse
        propagateControlFlow()

        val emptyHeap = heapImpl.newEmptyHeapState()
        val resultingHeap = endNode.resultingHeap
        _scriptReturnValue = UnionValue(returns.map(_.newSource().get()): _*).normalized
        globalHeapState = resultingHeap.map(garbageCollect(emptyHeap, _, _scriptReturnValue)._1).getOrElse(globalHeapState)
    }

    private def analyseFunction(function: FunctionValue): Boolean = {
        assert(nodesToPropagate.isEmpty)

        val context = knownFunctions(function)
        val callNode = context.callNode
        val returnSource = context.returnSource

        controlFlowTo(context.callNode, globalHeapState)

        // analyse
        propagateControlFlow()

        return context.fetchResultingHeap().exists {
            heap =>
                val oldReturn = context.returnValue
                val newReturn = UnionValue(context.returnValue, returnSource.get()).normalized
                context.returnValue = newReturn

                val (newHeap, hasChanged) = garbageCollect(globalHeapState, heap, context.returnValue)
                globalHeapState = newHeap

                hasChanged || !oldReturn.structureEquals(newReturn)
        }
    }


    def globalHeap: HeapMemory = globalHeapState
    def scriptReturnValue: ValueLike = _scriptReturnValue
    def scriptReturn: IniEntity = globalHeapState.toIniEntity(Seq(scriptReturnValue), this).head._2

    def analyse(): Unit = {
        analyseInitialScriptExecution()

        val queue = mutable.Queue.empty[FunctionValue]
        var changed = false

        do {
            queue ++= knownFunctions.keySet
            changed = false
            while (queue.nonEmpty) {
                val func = queue.dequeue()
                if (analyseFunction(func)) {
                    changed = true
                }
            }

        } while (changed)
    }

    override def unify(heaps: HeapMemory*): HeapMemory = heapImpl.unify(heaps: _*)

    override def getFunctionInfo(function: FunctionValue): Option[FunctionInfo] = knownFunctions.get(function)
}

object ForwardFlowAnalysis {

    def create(script: Ast.Script, solver: Solver, heapImpl: Heap, global: IniObject): ForwardFlowAnalysis = {
        val scriptTemplate = TemplateBuilder.buildScriptTemplate(script)
        return new ForwardFlowAnalysis(scriptTemplate, solver, heapImpl, global)
    }
}
