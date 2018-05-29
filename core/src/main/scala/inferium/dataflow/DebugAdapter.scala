package inferium.dataflow
import inferium.dataflow.graph.visitors.DebugCheckVisitor
import inferium.dataflow.graph.{Graph, Node}

abstract class DebugAdapter {
    def error(node: graph.Node, message: String): Unit
    def warn(node: graph.Node, message: String): Unit
    def info(node: graph.Node, message: String): Unit

    def finalizeAnalysis(beginNode: Node)(implicit analysis: DataFlowAnalysis): Unit = {
        val visitor = new DebugCheckVisitor(this)
        visitor.start(beginNode)
    }

    def hasError: Boolean
}

object DebugAdapter {
    object Empty extends DebugAdapter {
        override def error(node: Node, message: String): Unit = ()
        override def warn(node: Node, message: String): Unit = ()
        override def info(node: Node, message: String): Unit = ()

        override def finalizeAnalysis(beginNode: Node)(implicit analysis: DataFlowAnalysis): Unit = ()

        override def hasError: Boolean = false
    }
}