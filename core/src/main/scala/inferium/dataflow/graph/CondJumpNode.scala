package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

class CondJumpNode(val thenNode: Node, val elseNode: Node)(implicit _info: Node.Info) extends LinearNode {
    assert(thenNode != null)
    assert(elseNode != null)

    thenNode.addPredecessor(this)
    elseNode.addPredecessor(this)

    override def hasSucc: Boolean = true
    override val successors: Seq[Node] = Seq(thenNode, elseNode)

    override protected[graph] def removeSuccessor(node: Node): Unit = {
        throw new IllegalAccessException("CondJumpNodes do not have normal successor")
    }
    override protected[graph] def addSuccessor(node: Node): Unit = {
        throw new IllegalAccessException("CondJumpNodes do not have normal successor")
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        val cond :: rest = inState.stack

        // todo: handle cond
        val newState = inState.copy(stack = rest)
        thenNode <~ newState
        elseNode <~ newState
    }

    override def toString: String = s"cond[${thenNode.label},${elseNode.label}]"
    override def asAsmStmt: String = s"cond ${thenNode.label}, ${elseNode.label}"
}
