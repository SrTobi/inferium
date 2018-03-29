package inferium.dataflow.graph
import inferium.dataflow.ExecutionState

class JumpNode extends LinearNode {
    private var _target: Node = _

    def target: Node = {
        assert(_target != null)
        _target
    }

    def target_=(node: Node): Unit = {
        assert(_target == null)
        assert(node != null)
        _target = node
    }

    override def onControlFlow(state: ExecutionState): Unit = {
        val analysis = state.dataFlowAnalysis
        analysis.stateFlowsTo(state, target)
        analysis.noStateFlowsTo(next)
    }

    override def onNoControlFlow(): Unit = {

    }
}
