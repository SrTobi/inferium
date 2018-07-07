package inferium.dataflow.graph.traits

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.Node
import inferium.dataflow.graph.Node.CatchTarget
import inferium.lattice.Entity

trait Failing extends Origin {
    def catchTarget: CatchTarget

    protected def fail(state: ExecutionState, exception: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
        Node.fail(catchTarget, state, exception)(thisOrigin, analysis)
    }
}
