package inferium.dataflow.graph

abstract class LinearNode extends Node {
    private var _next: Node = _

    def hasNext: Boolean = _next != null

    def next: Node = {
        assert(_next != null)
        _next
    }

    def next_=(node: Node): Unit = {
        assert(_next == null)
        assert(node != null)
        _next = node
        node.addIncomingNode(node)
    }

    def connectToNext(node: LinearNode): Unit = {

    }
}
