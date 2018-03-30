package inferium.dataflow.graph

trait SinglePredecessor extends Node{
    private var _pred: Node = _

    def hasPred: Boolean = _pred != null

    def pred: Node = {
        assert(_pred != null)
        _pred
    }

    override def predecessors: Traversable[Node] = Option(_pred)

    protected override def addPredecessor(node: Node): Unit = {
        assert(node != null)
        assert(_pred == null, s"${this} can only have one predecessor")
        _pred = node
    }

    protected override def removePredecessor(node: Node): Unit = {
        assert(node != null)
        assert(node == _pred)
        _pred = null
    }
}
