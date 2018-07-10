package inferium.debug

import escalima.ECMAScript
import escalima.ast.Program
import inferium.Config
import inferium.dataflow.graph.{Node, ScriptGraph}
import inferium.dataflow._
import inferium.lattice.{Location, ObjectLike, UndefinedValue}
import inferium.lattice.heaps.SimpleHeap
import inferium.prelude.NodeJs

class FixtureRunner(val fixture: Fixture, val bridge: ECMAScript = new ECMAScript) {

    val name: String = fixture.name
    val description: String = fixture.description

    def config: Config = fixture.config

    val prog: Program = bridge.parseScript(fixture.code)

    val graph: ScriptGraph = new GraphBuilder(config).buildTemplate(prog).instantiate()

    val iniState: ExecutionState = NodeJs.initialState(config)
    val debugAdapter: DebugAdapter = new DebugAdapter {
        private var _hasError = false
        override def error(node: Node, message: String): Unit = {
            _hasError = true
            println("Error: " + message)
        }
        override def warn(node: Node, message: String): Unit = println("Warn: " + message)
        override def info(node: Node, message: String): Unit = println("Info: " + message)
        override def hasError: Boolean = _hasError
    }

    def run(): Boolean = {
        val analysis = new ScriptAnalysis(graph, debugAdapter)
        analysis.runAnalysis(iniState)
        return !debugAdapter.hasError
    }
}

object FixtureRunner {
    private lazy val bridge = new ECMAScript
    def test(code: String): Unit = {
        val baseConfig = Config(
            GraphBuilder.Config.buildDebugNodes := true
        )
        val fixture = Fixture.fromSource(code, baseConfig)
        val runner = new FixtureRunner(fixture, bridge)
        val runResult = runner.run()
        assert(runResult)
    }
}