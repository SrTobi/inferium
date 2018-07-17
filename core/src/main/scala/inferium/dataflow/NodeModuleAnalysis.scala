package inferium.dataflow

import inferium.dataflow.graph.Node
import inferium.dataflow.graph.Node.StateOrigin
import inferium.lattice.{Entity, Heap, UnionValue, _}

import scala.collection.mutable

class NodeModuleAnalysis(initialModuleCode: Analysable, val globalObject: ObjectLike, val debugAdapter: DebugAdapter = DebugAdapter.Empty) {


    private class CodeAnalysis(analysable: Analysable) extends DataFlowAnalysis(debugAdapter) {
        private implicit def useThisAsAnalysis: DataFlowAnalysis = this
        private val initialStateOrigin = StateOrigin()

        private val enquedNodes = mutable.Set.empty[graph.Node]
        private val nodesToProcess = mutable.PriorityQueue.empty[graph.Node](Ordering.by(_.priority))

        override def enqueue(node: graph.Node): Unit = {
            if (!enquedNodes(node)) {
                nodesToProcess.enqueue(node)
                enquedNodes += node
            }
        }

        def run(state: ExecutionState): ExecutionState = {
            analysable.begin.setNewInState(state, initialStateOrigin)

            while (nodesToProcess.nonEmpty) {
                val node = nodesToProcess.dequeue()
                enquedNodes -= node
                node.process(this)
            }

            analysable.end.inState
        }
    }

    private class FunctionAnalyser(analysable: Analysable) {

        private val argumentProbe = new ProbeEntity
        private val location = Location()
        private val analysis = new CodeAnalysis(analysable)
        private var lexicalFrame: LexicalFrame = _
        private var returnValue: Entity = NeverValue

        def addLexicalFrame(lexicalFrame: LexicalFrame): Unit = {
            if (this.lexicalFrame == null) {
                this.lexicalFrame = lexicalFrame
            } else {
                this.lexicalFrame = this.lexicalFrame unify lexicalFrame
            }
        }

        def run(): Boolean = {
            if (!userState.hasEffect(location)) {
                println("skipped")
                return false
            }

            val heap = userState.toHeap(location)
            val state = ExecutionState(argumentProbe :: Nil, heap, globalObject, lexicalFrame)
            val ExecutionState(resultStack, resultHeap, _, _) = analysis.run(state)
            val returnValue :: Nil = resultStack

            val unifiedReturnValue = this.returnValue unify returnValue
            val returnValueChanged = unifiedReturnValue != this.returnValue
            this.returnValue = unifiedReturnValue


            val changed = userState.feed(resultHeap) || returnValueChanged

            val inspector = new Inspector
            inspector.inspectEntity(unifiedReturnValue)
            inspector.inspectObject(globalObject)
            inspector.inspectProbe(argumentProbe)

            changed
        }
    }

    private class Module(code: Analysable,
                         val path: String,
                         val moduleCtx: ObjectLike,
                         val moduleObj: ObjectLike,
                         val exportsObj: ObjectLike) {
        def load(heap: Heap): Heap = {
            val analysis = new CodeAnalysis(code)

            val state = ExecutionState(UndefinedValue :: Nil, heap, globalObject, LexicalFrame(moduleCtx, None))
            val resultState = analysis.run(state)
            resultState.heap
        }
    }

    private val modules = mutable.Map.empty[String, Module]

    private def getModule(code: Analysable, path: String, initialHeap: Heap): (Module, Heap) = {
        modules.get(path) match {
            case Some(module) =>
                (module, initialHeap)
            case None =>
                val mutator = initialHeap.begin(Location())

                val moduleCtx = mutator.allocOrdinaryObject(Location())
                val moduleObj = mutator.allocOrdinaryObject(Location())
                val exportsObj = mutator.allocOrdinaryObject(Location())

                mutator.forceSetPropertyValue(moduleCtx, "module", Location(), moduleObj)
                mutator.forceSetPropertyValue(moduleCtx, "exports", Location(), exportsObj)
                mutator.forceSetPropertyValue(moduleObj, "exports", Location(), exportsObj)

                val heapAfterSetup = initialHeap.end(mutator)
                val module = new Module(code, path, moduleCtx, moduleObj, exportsObj)
                modules += path -> module

                val resultHeap = module.load(heapAfterSetup)

                (module, resultHeap)

        }
    }

    private var userState: GlobalHeap = _
    private val foundFunctions = mutable.Map.empty[CallableInfo.Anchor, FunctionAnalyser]
    private var analyzingList = mutable.ListBuffer.empty[FunctionAnalyser]

    def runAnalysis(heap: Heap): Unit = {
        val (mainModule, heapAfter) = getModule(initialModuleCode, "/main", heap)

        userState = heapAfter.createGlobalHeap()

        val inspector = new Inspector
        inspector.inspectObject(globalObject)
        inspector.inspectObject(mainModule.exportsObj)

        while (analyseFunctions()) {
            // just wait till the user state is stable
        }
    }

    private def analyseFunctions(): Boolean = {
        var changed = false

        val worklist = analyzingList
        analyzingList = mutable.ListBuffer.empty

        assert(worklist.size == foundFunctions.size)

        for (func <- worklist) {
            if (func.run()) {
                changed = true
                analyzingList.prepend(func)
            } else {
                analyzingList.append(func)
            }
        }

        changed
    }


    private class Inspector {
        private val accessor = userState.accessor
        private val foundObjects = mutable.Set.empty[ObjectLike]

        def inspectEntity(entity: Entity): Unit = {
            entity.normalized(accessor) match {
                case union: UnionValue =>
                    union.entities foreach { inspectEntity }

                case obj: ObjectLike =>
                    inspectObject(obj)

                case probe: ProbeEntity =>
                    inspectProbe(probe)

                case _ =>
            }
        }

        def inspectObject(obj: ObjectLike): Unit = {
            if (foundObjects(obj)) {
                return
            }

            foundObjects += obj

            obj match {
                case AnyEntity =>
                // nothing to do

                case func: FunctionEntity =>
                    val info = func.callableInfo
                    if (info.yieldsGraph) {
                        val analyser = foundFunctions.getOrElseUpdate(info.anchor, {
                            val analyser = new FunctionAnalyser(info.asAnalysable)
                            analyzingList.prepend(analyser)
                            analyser
                        })
                        analyser.addLexicalFrame(func.lexicalFrame)
                    }


                case obj: OrdinaryObjectEntity =>
                // nothing to do
            }

            accessor.getOwnProperties(obj) foreach {
                case (_, prop: AbstractProperty) => inspectEntity(prop.value)
                case (_, prop: ConcreteProperty) => inspectEntity(prop.normalizedValue(accessor))
            }
        }

        private val foundProbes = mutable.Set.empty[ProbeEntity]

        def inspectProbe(probe: ProbeEntity): Unit = {
            if (foundProbes(probe)) {
                return
            }

            foundProbes += probe
            probe.entities foreach { inspectEntity }
        }
    }
}

