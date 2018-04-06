package inferium.dataflow.graph

import inferium.dataflow.ExecutionState

class LexicalWriteNode(val varName: String)(implicit _info: Node.Info) extends FailingTransformerNode {
    override protected def transform(state: ExecutionState): Option[ExecutionState] = {
        // do the read and push it onto the stack
        ???
    }

    override def asAsmStmt: String = s"write $varName"
}
