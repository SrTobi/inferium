package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

class JumpNode(val target: Node)(implicit _info: Node.Info) extends Node with SingleSuccessor with SinglePredecessor {
    assert(target != null)
    target.addPredecessor(this)

    override def successors: Traversable[Node] = super.successors ++ Seq(target)

    override def setNewInState(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Unit = {
        target <~ state
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        throw new IllegalStateException("jump nodes should not be processed")
    }

    override def asAsmStmt: String = s"jmp ${target.label}"
}
