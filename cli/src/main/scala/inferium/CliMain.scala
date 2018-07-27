package inferium

import java.nio.file.{Files, Path, Paths}

import escalima.ECMAScript
import inferium.dataflow.graph.Node
import inferium.dataflow.{Analysable, DebugAdapter, GraphBuilder, NodeModuleAnalysis}
import inferium.js.types.TypeScriptPrinter
import inferium.js.types.js.Prelude
import inferium.lattice.heaps.ChainHeap
import inferium.prelude.NodeJs
import ujson.Js

object CliMain {

    private object SimpleDebugAdapter extends DebugAdapter {
        override def error(node: Node, message: String): Unit = println("Error: " + message)
        override def warn(node: Node, message: String): Unit = println("Warn: " + message)
        override def info(node: Node, message: String): Unit = println(message)
        override def hasError: Boolean = ???
    }

    private class Source(config: Config, modulePath: String, prelude: Prelude) extends NodeModuleAnalysis.ModuleSource {
        val (initialHeap, globalObject, typedModules, instantiator) = NodeJs.initialHeap(config, ChainHeap, Some(prelude))
        val bridge = new ECMAScript
        val initialPath: String = requireFind(modulePath, "./").getOrElse { throw new Exception(s"Can't find main js file in '$modulePath'") }

        override def requireFind(path: String, searched: String): Option[String] = {
            if(!searched.startsWith("./") && !searched.startsWith("../")) {
                return None
            }

            def loadAsFile(x: String): Option[String] = {
                val xJs = x + ".js"

                if (Files.isRegularFile(Paths.get(x))) {
                    Some(x)
                } else if (Files.isRegularFile(Paths.get(xJs))){
                    Some(xJs)
                } else {
                    None
                }
            }

            def loadAsIndex(x: String): Option[String] = {
                val xIndex = x + "/index.js"
                if (Files.isRegularFile(Paths.get(xIndex))) {
                    Some(xIndex)
                } else {
                    None
                }
            }

            def loadAsDir(x: String): Option[String] = {
                val xPackageJson = x + "/package.json"
                if (Files.isRegularFile(Paths.get(xPackageJson))) {
                    val packageSource = io.Source.fromFile(xPackageJson).mkString
                    val json = upickle.json.read(packageSource)
                    json.obj.get("main") flatMap  {
                        main =>
                            val m = x + "/" + main.str
                            loadAsFile(m) orElse {
                                loadAsIndex(m)
                            }
                    } orElse {
                        loadAsIndex(x)
                    }
                } else {
                    None
                }
            }

            val isFile = Files.isRegularFile(Paths.get(path))
            val target = (if (isFile) Paths.get(path).getParent else Paths.get(path)).resolve(searched).toString
            loadAsFile(target).orElse {
                loadAsDir(target)
            }
        }

        override def require(path: String): Option[Analysable] = {
            println(s"Parse module '$path'")
            val prog = try {
                val code = io.Source.fromFile(path).mkString
                bridge.parseModule(code)
            } catch {
                case _: Throwable =>
                    println("Failed to parse module!")
                    return None
            }
            val graph = new GraphBuilder(config).buildTemplate(prog, NodeModuleAnalysis.defaultModuleEnv, hasModule = true).instantiate()
            println("Done Parsing module.")
            Some(graph)
        }
    }

    def main(args: Array[String]): Unit = {

        val Array(initialPath, typesFile) = args

        println("======= Inferium - Tobias Kahlert ======")
        println("MainScript: " + initialPath)
        println("TypesFile:  " + typesFile)

        println()
        println("======= Get Prelude ======")
        val preludeSource = io.Source.fromFile(typesFile).mkString
        val prelude = Prelude.load(upickle.json.read(preludeSource).asInstanceOf[Js.Obj])

        println("Prelude contains following types:")
        println(prelude.modules.keys.mkString(", "))


        println()
        println("======= Initialize ======")

        val config = InferiumConfig.Env.Node
        val source = new Source(config,initialPath, prelude)

        println("Done.")
        println()

        println("======= Analysing =======")
        val analysis = new NodeModuleAnalysis(source, SimpleDebugAdapter)
        val resultType = analysis.runAnalysis(source.initialHeap)

        println("Done")

        println()
        println("======= Print Type Definition ======")
        println()
        val typeDefinition = TypeScriptPrinter.print(resultType)

        println(typeDefinition)
    }
}
