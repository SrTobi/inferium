package inferium.dataflow

import inferium.lattice.Entity
import inferium.lattice.Heap


case class ExecutionState(stack: ExecutionState.Stack, heap: Heap, lexicalFrame: LexicalFrame) {
    def merge(others: Seq[ExecutionState], fixpoint: Boolean): ExecutionState = {
        // merge stack
        val stacks = others map { _.stack }
        assert(stacks forall { _.length == stack.length })
        val resultStack = if (stack.nonEmpty) {
            assert(stacks forall { _.tail eq stack.tail })
            Entity.unify(stack.head +: stacks.map(_.head)) :: stack.tail
        } else Nil

        def otherHeaps = others map { _.heap }
        val resultHeap = if (fixpoint) heap.fixpointUnify(otherHeaps) else heap.unify(otherHeaps)

        val lexFrames = others.iterator map { _.lexicalFrame }
        assert(lexFrames forall { _ == lexicalFrame})
        ExecutionState(resultStack, resultHeap, lexicalFrame)
    }
}

object ExecutionState {
    type Stack = List[Entity]
}