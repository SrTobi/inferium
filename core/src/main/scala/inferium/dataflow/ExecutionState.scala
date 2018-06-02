package inferium.dataflow

import inferium.Unifiable
import inferium.lattice.{Entity, Heap, UnionValue}


case class ExecutionState(stack: ExecutionState.Stack, heap: Heap, thisEntity: Entity, lexicalFrame: LexicalFrame) extends Unifiable[ExecutionState] {

    override def unify(other: ExecutionState)(implicit fixpoint: Unifiable.Fixpoint): ExecutionState = unify(Seq(other))
    override def unify(others: Seq[ExecutionState])(implicit fixpoint: Unifiable.Fixpoint): ExecutionState = {
        // merge stack
        val stacks = others map { _.stack }
        assert(stacks forall { _.length == stack.length })
        val resultStack = if (stack.nonEmpty) {
            assert(stacks forall { _.tail eq stack.tail })
            Entity.unify(stack.head +: stacks.map(_.head)) :: stack.tail
        } else Nil

        def otherHeaps = others map { _.heap }
        val resultHeap = heap unify otherHeaps

        val resultThis = Entity.unify(thisEntity +: others.map { _.thisEntity })

        // check that all call frames are the same
        lazy val lexFrames = others.iterator map { _.lexicalFrame }
        assert(lexFrames forall { _ == lexicalFrame})

        ExecutionState(resultStack, resultHeap, resultThis, lexicalFrame)
    }

}

object ExecutionState {
    type Stack = List[Entity]
}