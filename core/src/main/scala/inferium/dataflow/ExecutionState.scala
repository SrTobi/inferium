package inferium.dataflow

import inferium.lattice.Entity
import inferium.lattice.Heap


case class ExecutionState(stack: ExecutionState.Stack, heap: Heap, lexicalFrame: LexicalFrame) {
    def merge(others: Seq[ExecutionState]): ExecutionState = {
        // merge stack
        val stacks = others map { _.stack }
        assert(stacks forall { _.length == stack.length })
        val resultStack = if (stack.nonEmpty) {
            assert(stacks forall { _.tail eq stack.tail })
            Entity.unify(stack.head +: stacks.map(_.head)) :: stack.tail
        } else Nil

        val resultHeap = heap.unify(others map { _.heap })

        val lexFrames = others.iterator map { _.lexicalFrame }
        assert(lexFrames forall { _ eq lexicalFrame})
        ExecutionState(resultStack, resultHeap, lexicalFrame)
    }
}

object ExecutionState {
    type Stack = List[Entity]
}