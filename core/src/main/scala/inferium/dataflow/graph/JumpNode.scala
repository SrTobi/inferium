package inferium.dataflow.graph
import inferium.dataflow.graph.traits.{SinglePredecessor, SingleSuccessor}
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.Location

class JumpNode(val target: Node)(implicit _info: Node.Info) extends Node with SingleSuccessor with SinglePredecessor {
    assert(target != null)
    target.addPredecessor(this)

    override def successors: Seq[Node] = super.successors ++ Seq(target)

    override def setNewInState(state: ExecutionState, origin: Location)(implicit analysis: DataFlowAnalysis): Unit = {
        target <~ state
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        throw new IllegalStateException("jump nodes should not be processed")
    }

    override def asAsmStmt: String = s"jmp ${target.label}"
}
