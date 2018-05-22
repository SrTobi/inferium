package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node

trait SinglePredecessor extends Node{
    private var _pred: Node = _

    def hasPred: Boolean = _pred != null

    def pred: Node = {
        assert(_pred != null)
        _pred
    }

    override def predecessors: Seq[Node] = Option(_pred).toSeq

    protected[graph] override def addPredecessor(node: Node): Unit = {
        assert(node != null)
        assert(_pred == null, s"${this} can only have one predecessor")
        _pred = node
    }

    protected[graph] override def removePredecessor(node: Node): Unit = {
        assert(node != null)
        assert(node == _pred)
        _pred = null
    }
}
