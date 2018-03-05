package inferium.dataflow

import inferium.lattice.Entity
import inferium.memory.Heap


class ExecutionContext {
    private val dataFlowAnalysis: DataFlowAnalysis = null
    private var _exprStack: List[Entity] = Nil

    def heap: Heap = ???
    def exprStack: List[Entity] = _exprStack

    def popExpr(): Entity = {
        val expr :: newStack = _exprStack
        _exprStack = newStack
        expr
    }

    def popTwoExpr(): (Entity, Entity) = {
        val fst :: snd :: newStack = _exprStack
        _exprStack = newStack
        (fst, snd)
    }

    def flowTo(node: graph.Node): Unit = {
        dataFlowAnalysis.contextFlowsTo(this, node)
    }
}
