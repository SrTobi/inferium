package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.collection.mutable


object Nodes {
    class Node {
        var next: Node = _
        protected var outHeap: HeapState = _

        final def activate(heapState: HeapState): Unit = ???
        final def activate(heapStateBuilder: HeapStateBuilder): Unit = activate(heapStateBuilder.end())

        // when a heap flows into the node
        def onActivate(heap: HeapStateBuilder): Unit = {}

        // when a the value identity of a handle changes
        def onHandleChange(): Unit = {}

        // when the value properties change
        def onValueChange(): Unit = {}
    }

    class BeginNode extends Node {
        override def onActivate(heap: HeapStateBuilder): Unit = next.activate(heap)
    }


    /*
     * - result has a lower bound on Val while target = { propertyName: (Val,) }
     * - target has an upper bound on { propertyName: (,result) }
     * - all subsequent reads to propertyName on target must yield the same Value
     */
    class PropertyRead(val target: ValueHandle, val propertyName: String)(heap: Heap) extends Node {
        private var targetReader: HandleReader = _
        val result: ValueHandle = heap.newValueHandle()

        override def onActivate(heap: HeapStateBuilder): Unit = {
            targetReader = heap.newHandleReader(target)
            heap.readProperty(target, propertyName, result)
        }

        override def onHandleChange(): Unit = {
            // writing on null or undefined ends the program
            if (!targetReader.read().throwsWhenWrittenOrReadOn) {
                next.activate(outHeap)
            }
        }
    }


    /*
     * - result has a lower bound on Val while target = { propertyName: (Val,) }
     * - target has an upper bound on { propertyName: (,result) }
     * - all subsequent reads to propertyName on target must yield the same Value
     */
    class LocalRead(val target: Value, val propertyName: String)(heap: Heap) extends Node {
        val result: ValueHandle = heap.newValueHandle()

        override def onActivate(heap: HeapStateBuilder): Unit = {
            heap.readLocal(target, propertyName, result)
        }
    }

    /*
     * - target has an upper bound on { propertyName: value }
     * - value has an upper bound on { propertyName: ... }
     */
    class PropertyWrite(val target: ValueHandle, val propertyName: String, val value: ValueHandle) extends Node {
        private var targetReader: HandleReader = _

        override def onActivate(heap: HeapStateBuilder): Unit = {
            targetReader = heap.newHandleReader(target)
            heap.writeProperty(target, propertyName, value)
        }

        override def onHandleChange(): Unit = {
            // writing on null or undefined ends the program
            if (!targetReader.read().throwsWhenWrittenOrReadOn) {
                next.activate(outHeap)
            }
        }
    }

    /*
     * - target has an upper bound on { propertyName: value }
     * - value has an upper bound on { propertyName: ... }
     */
    class LocalWrite(val target: Value, val propertyName: String, val value: ValueHandle) extends Node {

        override def onActivate(heap: HeapStateBuilder): Unit = {
            heap.writeLocal(target, propertyName, value)
        }
    }

    /*
     * - left/right have an upper bound on number
     * - the result has a lower bound on number or is the result of the subtraction of left and right
     */
    class Subtraction(val left: ValueHandle, val right: ValueHandle)(heap: Heap, solver: Solver) extends Node {
        private var leftReader: HandleReader = _
        private var rightReader: HandleReader = _
        private var resultWriter: HandleWriter = _
        val result: ValueHandle = heap.newValueHandle()

        private def leftValue = leftReader.read()
        private def rightValue = rightReader.read()

        override def onActivate(heap: HeapStateBuilder): Unit = {
            leftReader = heap.newHandleReader(left)
            rightReader = heap.newHandleReader(right)
            resultWriter = heap.newHandleWriter(result)
            outHeap = heap.end()
            next.activate(outHeap)
        }

        override def onHandleChange(): Unit = {
            leftValue.flowsTo(solver.number())
            rightValue.flowsTo(solver.number())
        }

        override def onValueChange(): Unit = {
            (leftValue, rightValue) match {
                case (ConcreteNumber(leftNum), ConcreteNumber(rightNum)) =>
                    resultWriter.write(solver.number((leftNum + rightNum).toString))
                case _ =>
                    resultWriter.write(solver.number())
            }
        }
    }

    /*
     * - the result has a lower bound on value
     */
    class Literal(val literal: Value)(heap: Heap) extends Node {
        private var resultWriter: HandleWriter = _
        val result: ValueHandle = heap.newValueHandle()

        override def onActivate(heap: HeapStateBuilder): Unit = {
            resultWriter = heap.newHandleWriter(result)
            resultWriter.write(literal)
            outHeap = heap.end()
            next.activate(outHeap)
        }
    }

    class NewObject()(heap: Heap, solver: Solver) extends Literal(solver.newEmptyObject())(heap)

    class Conditional(val cond: ValueHandle, val thenBranch: (Node, Node), val elseBranch: Option[(Node, Node)])(heap: Heap) extends Node {

        private var thenConnected: Boolean = false
        private var elseConnected: Boolean = false
        private var condReader: HandleReader = _
        private val endOfBranchNode = new Node {
            override def onActivate(heap: HeapStateBuilder): Unit = {
                Conditional.this.next.activate(Conditional.this.outHeap)
            }
        }
        private var heapAfterCond: HeapState = _

        outHeap = heap.newMergeHeapState()

        override def onActivate(heap: HeapStateBuilder): Unit = {
            condReader = heap.newHandleReader(cond)
            heapAfterCond = heap.end()
        }

        override def onValueChange(): Unit = {
            if (!thenConnected || !elseConnected) {
                condReader.read().asBool match {
                    case BoolLattice.Top =>
                        connectThenBranch()
                        connectElseBranch()
                    case BoolLattice.True =>
                        connectThenBranch()
                    case BoolLattice.False =>
                        connectElseBranch()
                    case BoolLattice.Bottom =>
                }
            }
        }

        private def connectThenBranch(): Unit = {
            if (!thenConnected) {
                val heapAfterThruthyfy = heapAfterCond.truthyfy(cond)
                connectBranch(thenBranch, heapAfterThruthyfy)
                thenConnected = true
            }
        }

        private def connectElseBranch(): Unit = {
            if (!elseConnected) {
                val heapAfterFalsyfy = heapAfterCond.falsyfy(cond)
                elseBranch match {
                    case Some(elseBra) =>
                        connectBranch(elseBra, heapAfterFalsyfy)
                    case None =>
                        endOfBranchNode.activate(heapAfterFalsyfy)
                }
                elseConnected = true
            }
        }

        private def connectBranch(branch: (Node, Node), heapState: HeapState): Unit = branch match {
            case (begin, end) =>
                end.next = endOfBranchNode
                begin.activate(heapState)
        }
    }

    class FunctionCall(val target: ValueHandle, val arguments: Seq[ValueHandle])(heap: Heap) extends Node {
        private var targetReader: HandleReader = _
        private val instantiated = mutable.Set.empty[FunctionValue]
        private val endOfBranchNode = new Node {
            override def onActivate(heap: HeapStateBuilder): Unit = {
                FunctionCall.this.next.activate(FunctionCall.this.outHeap)
            }
        }

        private val returnMerger = heap.newValueHandleMerger()
        def result: ValueHandle = returnMerger
        outHeap = heap.newMergeHeapState()

        override def onActivate(heap: HeapStateBuilder): Unit = {
            targetReader = heap.newHandleReader(target)
        }

        override def onValueChange(): Unit = {
            val functions = targetReader.read().asFunctions

            // TODO: use effects

            for (FunctionValue(template, closures) <- functions.filter(!instantiated.contains(_))) {
                template.instantiate(closures, arguments, endOfBranchNode, returnMerger)
            }

            if (functions.nonEmpty) {
                next.activate(outHeap)
            }
        }
    }
}
