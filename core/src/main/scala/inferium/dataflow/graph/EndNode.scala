package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}

class EndNode(implicit _info: Node.Info) extends LinearNode {
    override def hasSucc: Boolean = false
    override def successors: Seq[Node] = Seq()

    override protected[graph] def removeSuccessor(node: Node): Unit = throw new IllegalAccessException("EndNode doesn't have a successor")
    override protected[graph] def addSuccessor(node: Node): Unit = throw new IllegalAccessException("EndNode doesn't have a successor")

    override def process(implicit analysis: DataFlowAnalysis): Unit = ()

    def result: ExecutionState = inState

    override def asAsmStmt: String = "end"
}
