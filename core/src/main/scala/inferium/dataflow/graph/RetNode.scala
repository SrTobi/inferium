package inferium.dataflow.graph

import inferium.dataflow.calls.CallInstance
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.traits.LinearNode

class RetNode(val callSite: CallInstance.RecursionAble)(implicit _info: Node.Info) extends LinearNode {
    override def hasSucc: Boolean = false
    override def successors: Seq[Node] = Seq()

    override protected[graph] def removeSuccessor(node: Node): Unit = throw new IllegalAccessException("EndNode doesn't have a successor")
    override protected[graph] def addSuccessor(node: Node): Unit = throw new IllegalAccessException("EndNode doesn't have a successor")

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        val ExecutionState(result :: _, heap, _, _) = inState
        callSite.ret(result, heap, analysis)
    }

    def result: ExecutionState = inState

    override def asAsmStmt: String = "ret"
}
