package inferium.dataflow
import inferium.dataflow.graph.Node

abstract class DebugAdapter {
    def error(node: graph.Node, message: String): Unit
    def warn(node: graph.Node, message: String): Unit
}

object DebugAdapter {
    object Empty extends DebugAdapter {
        override def error(node: Node, message: String): Unit = ()
        override def warn(node: Node, message: String): Unit = ()
    }
}