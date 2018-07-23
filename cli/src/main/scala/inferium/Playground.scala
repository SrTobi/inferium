package inferium

import escalima.ECMAScript
import inferium.dataflow._
import inferium.dataflow.calls.NativeCall
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.visitors.{DotPrintVisitor, PrintVisitor}
import inferium.lattice.{Location, NumberValue}
import inferium.lattice.heaps.ChainHeap
import inferium.prelude.NodeJs

import scala.util.Random


object Playground {

    class TestDebugAdapter extends DebugAdapter {
        override def error(node: Node, message: String): Unit = println("Error: " + message)

        override def warn(node: Node, message: String): Unit = println("Warn: " + message)

        override def info(node: Node, message: String): Unit = println(message)

        override def hasError: Boolean = ???
    }

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
              |    p = path.resolve(p);
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
              |                made = sync(path.dirname(p), opts, made);
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
              |/*
              |    name: do-while loop
              |    desc: Do-while loops should work correctly
              | */
              |function test() {
              |   return test()
              |}
              |
              |var x = test()
            """.stripMargin

        val code = code1


        val bridge = new ECMAScript
        val prog = bridge.parseModule(code)

        val config = InferiumConfig.Env.NodeDebug
        val graph = new GraphBuilder(config).buildTemplate(prog, NodeModuleAnalysis.defaultModuleEnv, hasModule = true).instantiate()

        val (initialHeap, globalObject, modules, instantiator) = NodeJs.initialHeap(config, ChainHeap, addPrelude = true)
        /*val heap = {
            val mutator = initialHeap.begin(Location())

            mutator.forceSetPropertyValue(globalObject, "rand", Location(), NativeCall.createSimpleFunction("rand",
                (heap, ths, args, rest, analysis) => {
                    (heap, NumberValue)
                }, mutator)
            )

            initialHeap.end(mutator)
        }*/







        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))
        //println(PrintVisitor.print(graph, printMergeNodes = true, showStackInfo = true))
        val analysis = new NodeModuleAnalysis(graph, globalObject, modules, instantiator, new TestDebugAdapter)
        //val analysis = new ScriptAnalysis(graph, new TestDebugAdapter)

        analysis.runAnalysis(initialHeap)
        //analysis.runAnalysis(NodeJs.initialState(config))

        //println(PrintVisitor.print(graph, showStackInfo = true, showNodeInfo = true))

        //println("-------")
        println(new DotPrintVisitor(showStackInfo = false).start(graph))


    }

}
