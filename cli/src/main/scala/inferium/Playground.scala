package inferium

import escalima.ECMAScript
import inferium.dataflow._
import inferium.dataflow.graph.Node
import inferium.js.types.TypeScriptPrinter
import inferium.js.types.js.{Instantiator, Prelude}
import inferium.lattice.ObjectLike
import inferium.lattice.heaps.ChainHeap
import inferium.prelude.NodeJs
import inferium.prelude.data.NodeJsPreludeData


object Playground {

    class TestDebugAdapter extends DebugAdapter {
        override def error(node: Node, message: String): Unit = println("Error: " + message)

        override def warn(node: Node, message: String): Unit = println("Warn: " + message)

        override def info(node: Node, message: String): Unit = println(message)

        override def hasError: Boolean = ???
    }
/*
    def main(args: Array[String]): Unit = {
        val v1 = new IniObject(Map("test" -> (true, IniValue(UndefinedValue))), None)
        var v2: IniObject = null
        v2 = new IniObject(Map("test" -> (true, new IniRec({v2}))), None)

        var v3: IniObject = null
        v3 = new IniObject(Map("test" -> (true, new IniRec({v3}))), None)

        println(v2 == v3)
        println((v2 | v3.members("test")._2) == v2)
    }*/

    def main(args: Array[String]): Unit = {
        val code1 =
            """
              |var path = require('path');
              |var fs = require('fs');
              |var _0777 = parseInt('0777', 8);
              |
              |module.exports = mkdirP.mkdirp = mkdirP.mkdirP = mkdirP;
              |
              |function mkdirP (p, opts, f, made) {
              |    if (typeof opts === 'function') {
              |        f = opts;
              |        opts = {};
              |    }
              |    else if (!opts || typeof opts !== 'object') {
              |        opts = { mode: opts };
              |    }
              |
              |    var mode = opts.mode;
              |    var xfs = opts.fs || fs;
              |
              |    if (mode === undefined) {
              |        mode = _0777 & (~process.umask());
              |    }
              |    if (!made) made = null;
              |
              |    var cb = f || function () {};
              |    debug(process).print("path")
              |    p = path.resolve(p);
              |    debug(xfs).print("test")
              |    xfs.mkdir(p, mode, function (er) {
              |        if (!er) {
              |            made = made || p;
              |            return cb(null, made);
              |        }
              |        switch (er.code) {
              |            case 'ENOENT':
              |                mkdirP(path.dirname(p), opts, function (er, made) {
              |                    if (er) cb(er, made);
              |                    else mkdirP(p, opts, cb, made);
              |                });
              |                break;
              |
              |            // In the case of any other error, just see if there's a dir
              |            // there already.  If so, then hooray!  If not, then something
              |            // is borked.
              |            default:
              |                xfs.stat(p, function (er2, stat) {
              |                    // if the stat fails, then that's super weird.
              |                    // let the original error be the failure reason.
              |                    if (er2 || !stat.isDirectory()) cb(er, made)
              |                    else cb(null, made);
              |                });
              |                break;
              |        }
              |    });
              |}
              |
              |mkdirP.sync = function sync (p, opts, made) {
              |    if (!opts || typeof opts !== 'object') {
              |        opts = { mode: opts };
              |    }
              |
              |    var mode = opts.mode;
              |    var xfs = opts.fs || fs;
              |
              |    if (mode === undefined) {
              |        mode = _0777 & (~process.umask());
              |    }
              |    if (!made) made = null;
              |
              |    p = path.resolve(p);
              |
              |    try {
              |        xfs.mkdirSync(p, mode);
              |        made = made || p;
              |    }
              |    catch (err0) {
              |        switch (err0.code) {
              |            case 'ENOENT' :
              |                made = mkdirP.sync(path.dirname(p), opts, made);
              |                sync(p, opts, made);
              |                break;
              |
              |            // In the case of any other error, just see if there's a dir
              |            // there already.  If so, then hooray!  If not, then something
              |            // is borked.
              |            default:
              |                var stat;
              |                try {
              |                    stat = xfs.statSync(p);
              |                }
              |                catch (err1) {
              |                    throw err0;
              |                }
              |                if (!stat.isDirectory()) throw err0;
              |                break;
              |        }
              |    }
              |
              |    return made;
              |};
            """.stripMargin

        val code2 =
            """
              |var path = require("path")
              |exports.x = function(a) {
              |  return path.join(a, a)
              |}
            """.stripMargin

        val code = code2


        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val config = InferiumConfig.Env.NodeDebug
        val graph = new GraphBuilder(config).buildTemplate(prog, NodeModuleAnalysis.defaultModuleEnv, hasModule = true).instantiate()

        val prelude = Some(Prelude.load(NodeJsPreludeData.json))
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



        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = false))
        //println(PrintVisitor.print(graph, printMergeNodes = true, showStackInfo = true))
        val analysis = new NodeModuleAnalysis(source, new TestDebugAdapter)
        //val analysis = new ScriptAnalysis(graph, new TestDebugAdapter)

        val exports = analysis.runAnalysis(initialHeap)

        println("====================")
        println(TypeScriptPrinter.print(exports))
        //println(new TypeScriptPrinter(exports).print())
        //analysis.runAnalysis(NodeJs.initialState(config))

        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))

        //println("-------")
        //println(new DotPrintVisitor(showStackInfo = false).start(graph))


    }

}
