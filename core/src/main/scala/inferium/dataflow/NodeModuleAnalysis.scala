package inferium.dataflow

import inferium.dataflow.NodeModuleAnalysis.ModuleSource
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.Node.StateOrigin
import inferium.js.types.js
import inferium.js.types.js.Instantiator
import inferium.lattice.{Entity, Heap, UnionValue, _}
import inferium.prelude.NodeBuiltins
import inferium.typescript.IniEntity

import scala.collection.mutable

class NodeModuleAnalysis(val source: ModuleSource,
                         val debugAdapter: DebugAdapter = DebugAdapter.Empty) {

    private def globalObject = source.globalObject

    private class CodeAnalysis(analysable: Analysable, module: Option[Module]) extends DataFlowAnalysis(debugAdapter) {
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

        def run(state: ExecutionState): Option[ExecutionState] = {
            analysable.begin.setNewInState(state, initialStateOrigin)

            while (nodesToProcess.nonEmpty) {
                val node = nodesToProcess.dequeue()
                enquedNodes -= node
                node.process(this)
            }

            Option(analysable.end.inState)
        }

        override def instantiator: Instantiator = source.instantiator

        override def requireModule(name: String, heap: Heap): Option[(Entity, Heap)] = {
            source.typedModules.get('"' + name + '"') map { (_, heap) } orElse[(Entity, Heap)] {
                module.flatMap {
                    m =>
                        source.requireFind(m.path, name)

                } flatMap {
                    path =>
                        source.require(path) flatMap  {
                            code =>
                                getModule(code, path, heap) map {
                                    case (foundModule, resultHeap) =>
                                        (foundModule.exportObject getOrElse { throw new Exception("Can not analyze recursive module!")}, resultHeap)
                                }
                        }
                }
            }
        }
    }

    private class FunctionAnalyser(val info: CallableInfo) {
        assert(info.yieldsGraph)
        private val location = Location()
        private val analysis = new CodeAnalysis(info.asAnalysable, None)
        private var lexicalFrame: LexicalFrame = _

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
            println("Analyse " + info.name)

            val heap = userState.toHeap(location)
            val state = ExecutionState(info.argumentProbe :: Nil, heap, info.thisProbe, lexicalFrame)
            analysis.run(state) match {
                case Some(ExecutionState(resultStack, resultHeap, _, _)) =>
                    val unnormalizedReturnValue :: Nil = resultStack
                    val returnValue = unnormalizedReturnValue.normalized(resultHeap.begin(Location()))

                    val unifiedReturnValue = info.returnValue unify returnValue
                    val returnValueChanged = unifiedReturnValue != info.returnValue
                    info.returnValue = unifiedReturnValue


                    val changed = userState.feed(resultHeap) || returnValueChanged

                    val inspector = new Inspector
                    inspector.inspectEntity(unifiedReturnValue)
                    inspector.inspectObject(globalObject)
                    inspector.inspectProbe(info.thisProbe)
                    inspector.inspectProbe(info.argumentProbe)

                    changed

                case None =>
                    false
            }
        }
    }

    private class Module(code: Analysable,
                         val path: String,
                         val moduleCtx: ObjectLike,
                         val moduleObj: ObjectLike,
                         val initialExportObj: ObjectLike) {
        var exportObject: Option[Entity] = None

        def load(heap: Heap): Option[Heap] = {
            val analysis = new CodeAnalysis(code, Some(this))

            val state = ExecutionState(UndefinedValue :: Nil, heap, globalObject, moduleCtx :: LexicalFrame(globalObject, None))
            val resultState = analysis.run(state)
            resultState map {
                state =>
                    assert(exportObject.isEmpty)
                    val accessor = state.heap.begin(Location())
                    exportObject = Some(accessor.getProperty(moduleObj, "exports").abstractify(accessor).value)
                    state.heap
            }
        }
    }

    private val modules = mutable.Map.empty[String, Module]

    private def getModule(code: => Analysable, path: String, initialHeap: Heap): Option[(Module, Heap)] = {
        modules.get(path) match {
            case Some(module) =>
                Some(module -> initialHeap)
            case None =>
                val mutator = initialHeap.begin(Location())

                val moduleCtx = mutator.allocOrdinaryObject(Location())
                val moduleObj = mutator.allocOrdinaryObject(Location())
                val exportsObj = mutator.allocOrdinaryObject(Location())
                val require = mutator.allocBuiltin(NodeBuiltins.require)

                mutator.forceSetPropertyValue(moduleCtx, "module", Location(), moduleObj)

                mutator.forceSetPropertyValue(moduleCtx, "exports", Location(), exportsObj)
                mutator.forceSetPropertyValue(moduleObj, "exports", Location(), exportsObj)

                mutator.forceSetPropertyValue(moduleCtx, "require", Location(), require)
                mutator.forceSetPropertyValue(moduleObj, "require", Location(), require)

                val heapAfterSetup = initialHeap.end(mutator)
                val module = new Module(code, path, moduleCtx, moduleObj, exportsObj)
                modules += path -> module

                module.load(heapAfterSetup) map (module -> _)

        }
    }

    private var userState: GlobalHeap = _
    private val foundFunctions = mutable.Map.empty[CallableInfo.Anchor, FunctionAnalyser]
    private var analyzingList = mutable.ListBuffer.empty[FunctionAnalyser]

    def runAnalysis(heap: Heap): js.Type = {
        val mainPath = source.initialPath
        val mainCode = source.require(mainPath) getOrElse { throw new Exception(s"Can not find main module '$mainPath'") }
        val (mainModule, heapAfter) = getModule(mainCode, mainPath, heap).getOrElse(throw new Exception("Failed to analyze main module!"))

        userState = heapAfter.createGlobalHeap()

        val exports = mainModule.exportObject.get
        val inspector = new Inspector
        inspector.inspectObject(globalObject)
        inspector.inspectEntity(exports)

        var i = 0
        do {
            // just wait till the user state is stable
            println(s"Round $i")
            i += 1
        } while (analyseFunctions())

        val acc = userState.accessor//.begin(Location()) //.accessor
        js.from(exports, acc)
    }

    private def analyseFunctions(): Boolean = {
        var changed = false

        val worklist = analyzingList
        analyzingList = mutable.ListBuffer.empty

        assert(worklist.size == foundFunctions.size)

        for (func <- worklist) {
            if (func.run()) {
                changed = true
            }
            analyzingList.append(func)
        }

        changed
    }


    private class Inspector {
        private val accessor = userState.accessor
        private val foundObjects = mutable.Set.empty[ObjectLike]

        def inspectEntity(entity: Entity): Unit = {
            assert(entity.isNormalized)
            entity match {
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
                            println("Found " + info.name)
                            val analyser = new FunctionAnalyser(info)
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
            //probeprobe.entities foreach { inspectEntity }
        }
    }
}

object NodeModuleAnalysis {

    trait ModuleSource {
        def initialPath: String
        def instantiator: Instantiator
        def globalObject: ObjectLike
        def typedModules: Map[String, ObjectLike]

        def requireFind(path: String, searched: String): Option[String]
        def require(path: String): Option[Analysable]
    }


    val defaultModuleEnv: Map[String, String] = Seq("exports", "module", "require").map(n => n -> n).toMap
}