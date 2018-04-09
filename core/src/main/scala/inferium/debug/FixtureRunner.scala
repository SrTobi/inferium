package inferium.debug

import escalima.ECMAScript
import escalima.ast.Program
import inferium.dataflow.graph.ScriptGraph
import inferium.dataflow._
import inferium.lattice.{Location, ObjLocation, UndefinedValue}
import inferium.lattice.heaps.SimpleHeap

class FixtureRunner(val fixture: Fixture, val bridge: ECMAScript = new ECMAScript) {

    val name: String = fixture.name
    val description: String = fixture.description

    val prog: Program = bridge.parseScript(fixture.code)

    val graph: ScriptGraph = new GraphBuilder(fixture.config).buildTemplate(prog).instantiate()

    val globalObj: ObjLocation = ObjLocation(Location())
    val iniState: ExecutionState = new ExecutionState(UndefinedValue :: Nil, new SimpleHeap(), LexicalFrame(globalObj))
    val debugAdapter: DebugAdapter.Empty.type = DebugAdapter.Empty

    def run(): Boolean = {
        val analysis = new DataFlowAnalysis(graph, debugAdapter)
        analysis.runAnalysis(iniState)
        return !debugAdapter.hasError
    }
}

object FixtureRunner {
    def test(code: String): Unit = {
        val fixture = Fixture.fromSource(code)
        val runner = new FixtureRunner(fixture)
        val runResult = runner.run()
        assert(runResult)
    }
}