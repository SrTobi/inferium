package inferium.dataflow.graph.visitors

import inferium.dataflow.{DataFlowAnalysis, DebugAdapter}
import inferium.dataflow.graph.{DebugNode, Node}

class DebugCheckVisitor(debugAdapter: DebugAdapter)(implicit analysis: DataFlowAnalysis) extends Node.AllVisitor {
    override protected def visit(node: Node): Unit = node match {
        case node: DebugNode =>
            node.executeChecks(debugAdapter)

        case _ =>
    }
}
