package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.ForwardFlowAnalysis.IniObject
import com.github.srtobi.inferium.prototype.{Ast, LangParser}
import fastparse.core.Parsed

import scala.collection.mutable


class ForwardFlowAnalysis private(val scriptTemplate: Templates.Script, override val solver: Solver, val heap: Heap, global: IniObject) extends FlowAnalysis {

    import Nodes.Node

    private val endNode = new Node()(this) {
        override def onControlFlow(heap: HeapMemory): Unit = {
            lastMemory = heap
            assert(nodesToPropagate.isEmpty)
        }
        override def onNoControlFlow(): Unit = {
            lastMemory = null
            assert(nodesToPropagate.isEmpty)
        }
    }
    private val startHeapState = heap.newEmptyHeapState()
    private var lastMemory: HeapMemory = startHeapState
    private val globalObject = ForwardFlowAnalysis.writeIniObjectToHeap(startHeapState, global)
    private val mergeNode = new Nodes.MergeNode(0, endNode)(this)
    private val nodesToPropagate = mutable.Queue.empty[(Node, Option[HeapMemory])]
    private val (beginNode, returns) = scriptTemplate.instantiate(this, globalObject, mergeNode)

    mergeNode.setNumBranchesToWaitFor(returns.length)
    controlFlowTo(beginNode, startHeapState)


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

    def lastHeap: Option[HeapMemory] = Option(lastMemory)
    def scriptReturn: ValueLike = UnionValue(returns.map(_.newSource().get()): _*)

    def analyse(): Unit = {
        // analyse initial code
        assert(nodesToPropagate.nonEmpty)
        propagateControlFlow()
    }

    override def unify(heaps: HeapMemory*): HeapMemory = heap.unify(heaps: _*)
}

object ForwardFlowAnalysis {
    class IniObject(val members: scala.collection.Map[String, PropertyValue])

    object IniObject {
        private def toPropertyValue(any: Any): PropertyValue = any match {
            case obj: IniObject => Left(obj)
            case anything => Right(Value(anything))
        }
        def apply(valueMembers: (String, Any)*): IniObject = new IniObject(Map(valueMembers.map { case (prop, value) => (prop, toPropertyValue(value))}: _*))
    }

    type PropertyValue = Either[IniObject, ValueLike]

    private def writeIniObjectToHeap(heap: HeapMemory, obj: IniObject, objMap: mutable.Map[IniObject, ObjectValue] = mutable.Map.empty[IniObject, ObjectValue]): ObjectValue = {
        return objMap.getOrElse(obj, {
            val objValue = new ObjectValue
            objMap += (obj -> objValue)
            obj.members foreach {
                case (prop, member) =>
                    val value = member match {
                        case Left(objMember) => writeIniObjectToHeap(heap, objMember, objMap)
                        case Right(valueMember) => valueMember
                    }
                    heap.writeProperty(objValue, prop, value)
            }
            objValue
        })
    }

    def create(script: Ast.Script, solver: Solver, heap: Heap, global: IniObject): ForwardFlowAnalysis = {
        val scriptTemplate = TemplateBuilder.buildScriptTemplate(script)
        return new ForwardFlowAnalysis(scriptTemplate, solver, heap, global)
    }
}
