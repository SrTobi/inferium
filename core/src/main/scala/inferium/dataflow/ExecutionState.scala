package inferium.dataflow

import inferium.lattice.Entity
import inferium.lattice.Heap


case class ExecutionState(stack: ExecutionState.Stack, heap: Heap, callFrame: ExecutionState.CallFrame) {

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

        // check that all call frames are the same
        lazy val otherCallFrames = others.iterator map { _.callFrame }
        assert(otherCallFrames forall { _ == callFrame })

        ExecutionState(resultStack, resultHeap, callFrame)
    }

    def lexicalFrame: LexicalFrame = callFrame.lexicalFrame
    def thisEntity: Entity = callFrame.thisEntity

    def withCallFrame(callFrame: ExecutionState.CallFrame): ExecutionState = copy(callFrame = callFrame)
    def withLexicalFrame(lexicalFrame: LexicalFrame): ExecutionState = withCallFrame(callFrame.copy(lexicalFrame = lexicalFrame))
}

object ExecutionState {
    case class CallFrame(thisEntity: Entity, lexicalFrame: LexicalFrame, next: Option[CallFrame]) {
        val depth: Int = next map { _.depth + 1 } getOrElse 0

        override def toString: String = s"($thisEntity, $lexicalFrame)${next map {" :: " + _ } getOrElse ""}"
    }

    type Stack = List[Entity]
}