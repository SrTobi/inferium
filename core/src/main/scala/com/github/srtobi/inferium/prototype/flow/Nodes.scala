package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice


object Nodes {
    trait Node {
        protected def uses(value: ValueProvider*): Unit = value.foreach(_.usedBy(this))
        protected def unused(value: ValueProvider*): Unit = value.foreach(_.notUsedBy(this))

        def inputChanged(): Unit = ()
    }

    class Function(returnValue: ValueProvider, inHeap: HeapState, outHeap: HeapState)

    /*
     * - result has a lower bound on Val while target = { propertyName: (Val,) }
     * - target has an upper bound on { propertyName: (,result) }
     * - all subsequent reads to propertyName on target must yield the same Value
     */
    class PropertyRead(val target: ValueProvider, val propertyName: String)(heap: Heap, state: HeapState) extends Node {
        private val resultSubmitter = heap.newValueSubmitter()
        private val heapReader = heap.newHeapReader(state, propertyName)

        uses(target)

        override def inputChanged(): Unit = {
            val result = heapReader.read(target.fetch())
            resultSubmitter.submit(result)
        }

        def result: ValueProvider = resultSubmitter
    }

    /*
     * - target has an upper bound on { propertyName: value }
     * - value has an upper bound on { propertyName: ... }
     */
    class PropertyWrite(val target: ValueProvider, val propertyName: String, val value: ValueProvider)(heap: Heap, inState: HeapState, outState: HeapState) extends Node {
        private val resultSubmitter = heap.newValueSubmitter()
        private val heapWriter = heap.newHeapWriter(inState, outState, propertyName)

        uses(target, value)

        override def inputChanged(): Unit = {
            val result = heapWriter.write(target.fetch(), value.fetch())
            resultSubmitter.submit(result)
        }
    }

    /*
     * - left/right have an upper bound on number
     * - the result has a lower bound on number or is the result of the subtraction of left and right
     */
    class Subtraction(val left: ValueProvider, val right: ValueProvider)(heap: Heap, solver: Solver) extends Node {
        private val resultSubmitter = heap.newValueSubmitter()

        uses(left, right)

        override def inputChanged(): Unit = {
            left.foreach(_ flowsTo solver.number())
            right.foreach(_ flowsTo solver.number())

            // TODO: if both left and right are concrete values submit the subtraction result
            resultSubmitter.submit(solver.number())
        }

        def result: ValueProvider = resultSubmitter
    }

    /*
     * - the result has a lower bound on value
     */
    class Literal(val literal: Value)(heap: Heap) extends Node {
        private val resultSubmitter = heap.newValueSubmitter()

        resultSubmitter.submit(literal)

        def result: ValueProvider = resultSubmitter
    }

    class NewObject()(heap: Heap, solver: Solver) extends Node {
        private val resultSubmitter = heap.newValueSubmitter()

        resultSubmitter.submit(solver.newEmptyObject())

        def result: ValueProvider = resultSubmitter
    }

    class Conditional(val cond: ValueProvider, val thenBranch: (HeapState, HeapState), val elseBranch: Option[(HeapState, HeapState)])
                     (heap: Heap, inState: HeapState, outState: HeapState) extends Node {

        uses(cond)

        override def inputChanged(): Unit = {
            cond.asOption.map(_.boolLike).getOrElse(BoolLattice.Bottom) match {
                case BoolLattice.Top =>
                    connectThenBranch()
                    connectElseBranch()
                    unused(cond)
                case BoolLattice.True =>
                    connectThenBranch()
                case BoolLattice.False =>
                    connectElseBranch()
            }
        }

        private def connectThenBranch(): Unit = {
            connectBranch(thenBranch)
        }

        private def connectElseBranch(): Unit = {
            connectBranch(elseBranch.getOrElse((inState, outState)))
        }

        private def connectBranch(branch: (HeapState, HeapState)): Unit = {
            val (in, out) = branch
            heap.addHeapFlow(inState, in)
            heap.addHeapFlow(out, outState)
        }
    }

    class FunctionCall(val target: ValueProvider, val arguments: Seq[ValueProvider]) extends Node {
        uses(target)

        override def inputChanged(): Unit = {
            val functions = ??? // TODO: get all functions out of target

            // TODO: for each function decide which overloads ar applicable
            // TODO: then decide whether to use effects or to inline the function
            // TODO: - instantiate each function and inline it
            // TODO: - use effects on heap
        }
    }
}
