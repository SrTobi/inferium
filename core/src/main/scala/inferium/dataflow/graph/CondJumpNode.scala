package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.Location
import inferium.lattice.assertions.{Assertion, Falsyfied, Truthyfied}

class CondJumpNode(val thenNode: Node, val elseNode: Node)(implicit _info: Node.Info) extends LinearNode {
    assert(thenNode != null)
    assert(elseNode != null)

    thenNode.addPredecessor(this)
    elseNode.addPredecessor(this)

    private val truthyfyLoc: Location = Location()
    private val falsyfyLoc: Location = Location()

    override def hasSucc: Boolean = true
    override val successors: Seq[Node] = Seq(thenNode, elseNode)

    override protected[graph] def removeSuccessor(node: Node): Unit = {
        throw new IllegalAccessException("CondJumpNodes do not have normal successor")
    }
    override protected[graph] def addSuccessor(node: Node): Unit = {
        throw new IllegalAccessException("CondJumpNodes do not have normal successor")
    }

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        val cond :: rest = inState.stack
        val heap = inState.heap

        val condBool = cond.asBoolLattice(heap.begin(loc))

        def afterAssert(assertion: Assertion, l: Location) = {
            val mutator = heap.begin(l)
            cond.instituteAssertion(assertion, mutator)
            inState.copy(stack = rest, heap = heap.end(mutator))
        }

        if (condBool.mightBeTrue)
            thenNode <~ afterAssert(Truthyfied, truthyfyLoc)

        if (condBool.mightBeFalse)
            elseNode <~ afterAssert(Falsyfied, falsyfyLoc)
    }

    override def asAsmStmt: String = s"cond ${thenNode.label}, ${elseNode.label}"
}
