package inferium

import escalima.ECMAScript
import inferium.dataflow.graph.Node
import inferium.dataflow.{Analysable, DebugAdapter, GraphBuilder, NodeModuleAnalysis}
import inferium.js.types.TypeScriptPrinter
import inferium.js.types.js.{Instantiator, Prelude}
import inferium.lattice.ObjectLike
import inferium.lattice.heaps.ChainHeap
import inferium.prelude.NodeJs
import inferium.prelude.data.NodeJsPreludeData
import ujson.Js

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("InferiumSimple")
object SimpleApi {
    private lazy val bridge = new ECMAScript
    private var prelude: Option[Prelude] = None

    private class TestDebugAdapter extends DebugAdapter {
        override def error(node: Node, message: String): Unit = println("Error: " + message)
        override def warn(node: Node, message: String): Unit = println("Warn: " + message)
        override def info(node: Node, message: String): Unit = println(message)
        override def hasError: Boolean = ???
    }

    @JSExport
    def setPrelude(preludeData: String): Unit = {
        val json = upickle.json.read(preludeData).asInstanceOf[Js.Obj]
        prelude = Some(Prelude.load(json))
    }

    @JSExport
    def generateTypeDefinition(code: String): String = {


        val prog = bridge.parseModule(code)

        val config = InferiumConfig.Env.NodeDebug
        val graph = new GraphBuilder(config).buildTemplate(prog, NodeModuleAnalysis.defaultModuleEnv, hasModule = true).instantiate()

        val (initialHeap, gObj, modules, inster) = NodeJs.initialHeap(config, ChainHeap, prelude)
        /*val heap = {
            val mutator = initialHeap.begin(Location())

            mutator.forceSetPropertyValue(globalObject, "rand", Location(), NativeCall.createSimpleFunction("rand",
                (heap, ths, args, rest, analysis) => {
                    (heap, NumberValue)
                }, mutator)
            )

            initialHeap.end(mutator)
        }*/




        val source = new NodeModuleAnalysis.ModuleSource {
            override def initialPath: String = "$main$"

            override def instantiator: Instantiator = inster

            override def globalObject: ObjectLike = gObj

            override def typedModules: Map[String, ObjectLike] = modules

            override def requireFind(path: String, searched: String): Option[String] = None

            override def require(path: String): Option[Analysable] = if (path == initialPath) Some(graph) else None
        }



        val analysis = new NodeModuleAnalysis(source, new TestDebugAdapter)

        val exports = analysis.runAnalysis(initialHeap)

        TypeScriptPrinter.print(exports)
    }
}
