package inferium.dataflow.graph

trait SingleSuccessor extends Node {
    private var _succ: Node = _

    def hasSucc: Boolean = _succ != null

    def succ: Node = {
        assert(_succ != null)
        _succ
    }

    override def successors: Seq[Node] = Option(_succ).toSeq

    protected[graph] override def addSuccessor(node: Node): Unit = {
        assert(node != null)
        assert(_succ == null, s"${this} can only have one successor")
        _succ = node
    }

    protected[graph] override def removeSuccessor(node: Node): Unit = {
        assert(node != null)
        assert(node == _succ)
        _succ = null
    }
}
