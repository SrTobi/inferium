package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.annotation.tailrec
import scala.collection.mutable


object Nodes {
    import Heap.{State, ValueHandle, HandleReader, HandleWriter}

    class Node()(implicit val flowAnalysis: FlowAnalysis) {
        private var nextNode: Node = _

        def next: Node = nextNode
        def next_=(node: Node): Unit = {
            assert(nextNode == null)
            nextNode = node
        }
        private var _outHeap: State = _

        def outHeap: State = _outHeap
        protected def outHeap_=(heapState: State): Unit = {
            assert(_outHeap == null)
            _outHeap = heapState
        }

        def heap: Heap = flowAnalysis.heap
        def solver: Solver = flowAnalysis.solver

        protected final def activate(node: Node, heapState: State): Unit = flowAnalysis.activate(node, heapState)
        protected final def activate(node: Node, heapStateBuilder: HeapStateBuilder): Unit = activate(node, heapStateBuilder.end())

        def doActivation(inHeap: State): Unit = {
            val builder = new HeapStateBuilder(inHeap, this)
            onActivate(builder)
            if (outHeap == null) {
                outHeap = builder.end()
            } else {
                assert(builder.hasEnded)
            }
            assert(outHeap != null)
        }

        // when a heap flows into the node
        def onActivate(heap: HeapStateBuilder): Unit = {}

        // when a the value identity of a handle changes
        def onHandleChange(): Unit = {}

        // when the value properties change
        def onValueChange(): Unit = {}
    }

    class BeginNode(implicit flowAnalysis: FlowAnalysis) extends Node {
        override def onActivate(heapFlow: HeapStateBuilder): Unit = activate(next, heapFlow)
    }


    /*
     * - result has a lower bound on Val while target = { propertyName: (Val,) }
     * - target has an upper bound on { propertyName: (,result) }
     * - all subsequent reads to propertyName on target must yield the same Value
     */
    class PropertyRead(val target: ValueHandle, val propertyName: String)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private var targetReader: HandleReader = _
        val result: ValueHandle = heap.newValueHandle()

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            targetReader = heapFlow.newHandleReader(target)
            heapFlow.readProperty(target, propertyName, result)
        }

        override def onHandleChange(): Unit = {
            // writing on null or undefined ends the program
            if (!targetReader.read().throwsWhenWrittenOrReadOn) {
                activate(next, outHeap)
            }
        }
    }


    /*
     * - result has a lower bound on Val while target = { propertyName: (Val,) }
     * - target has an upper bound on { propertyName: (,result) }
     * - all subsequent reads to propertyName on target must yield the same Value
     */
    class LocalRead(val target: EmptyObject, val propertyName: String)(implicit flowAnalysis: FlowAnalysis) extends Node {
        val result: ValueHandle = heap.newValueHandle()

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            heapFlow.readLocal(target, propertyName, result)
        }
    }

    /*
     * - target has an upper bound on { propertyName: value }
     * - value has an upper bound on { propertyName: ... }
     */
    class PropertyWrite(val target: ValueHandle, val propertyName: String, val value: ValueHandle)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private var targetReader: HandleReader = _

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            targetReader = heapFlow.newHandleReader(target)
            heapFlow.writeProperty(target, propertyName, value)
        }

        override def onHandleChange(): Unit = {
            // writing on null or undefined ends the program
            if (!targetReader.read().throwsWhenWrittenOrReadOn) {
                activate(next, outHeap)
            }
        }
    }

    /*
     * - target has an upper bound on { propertyName: value }
     * - value has an upper bound on { propertyName: ... }
     */
    class LocalWrite(val target: EmptyObject, val propertyName: String, val value: ValueHandle)(implicit flowAnalysis: FlowAnalysis) extends Node {

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            heapFlow.writeLocal(target, propertyName, value)
        }
    }

    /*
     * - left/right have an upper bound on number
     * - the result has a lower bound on number or is the result of the subtraction of left and right
     */
    class Subtraction(val left: ValueHandle, val right: ValueHandle)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private var leftReader: HandleReader = _
        private var rightReader: HandleReader = _
        private var resultWriter: HandleWriter = _
        val result: ValueHandle = heap.newValueHandle()

        private def leftValue = leftReader.read()
        private def rightValue = rightReader.read()

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            leftReader = heapFlow.newHandleReader(left)
            rightReader = heapFlow.newHandleReader(right)
            resultWriter = heapFlow.newHandleWriter(result)
            outHeap = heapFlow.end()
            activate(next, outHeap)
        }

        override def onHandleChange(): Unit = {
            leftValue.flowsTo(solver.number())
            rightValue.flowsTo(solver.number())
        }

        override def onValueChange(): Unit = {
            (leftValue, rightValue) match {
                case (NumberValue(leftNum), NumberValue(rightNum)) =>
                    resultWriter.write(solver.number((leftNum + rightNum).toString))
                case _ =>
                    resultWriter.write(solver.number())
            }
        }
    }

    /*
     * - the result has a lower bound on value
     */
    class Literal(val literal: Value)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private var resultWriter: HandleWriter = _
        val result: ValueHandle = heap.newValueHandle()

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            resultWriter = heapFlow.newHandleWriter(result)
            resultWriter.write(literal)
            outHeap = heapFlow.end()
            activate(next, outHeap)
        }
    }

    class NewObject()(implicit flowAnalysis: FlowAnalysis) extends Literal(flowAnalysis.solver.newEmptyObject())

    class Conditional(val cond: ValueHandle, val thenBranch: (Node, Node), val elseBranch: Option[(Node, Node)])(implicit flowAnalysis: FlowAnalysis) extends Node {

        private var thenConnected: Boolean = false
        private var elseConnected: Boolean = false
        private var condReader: HandleReader = _
        private val endOfBranchNode = new Node {
            override def onActivate(heapFlow: HeapStateBuilder): Unit = {
                activate(Conditional.this.next, Conditional.this.outHeap)
            }
        }
        private var heapAfterCond: State = _

        outHeap = heap.newMergeHeapState()

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            condReader = heapFlow.newHandleReader(cond)
            heapAfterCond = heapFlow.end()
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
                        activate(endOfBranchNode, heapAfterFalsyfy)
                }
                elseConnected = true
            }
        }

        private def connectBranch(branch: (Node, Node), heapState: State): Unit = branch match {
            case (begin, end) =>
                end.next = endOfBranchNode
                activate(begin, heapState)
        }
    }

    class FunctionCall(val target: ValueHandle, val arguments: Seq[ValueHandle])(implicit flowAnalysis: FlowAnalysis) extends Node {
        private var targetReader: HandleReader = _
        private val instantiated = mutable.Set.empty[FunctionValue]
        private val endOfBranchNode = new Node {
            override def onActivate(heapFlow: HeapStateBuilder): Unit = {
                activate(FunctionCall.this.next, FunctionCall.this.outHeap)
            }
        }

        private val returnMerger = heap.newValueHandleMerger()
        def result: ValueHandle = returnMerger
        outHeap = heap.newMergeHeapState()

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            targetReader = heapFlow.newHandleReader(target)
        }

        override def onValueChange(): Unit = {
            val functions = targetReader.read().asFunctions

            // TODO: use effects

            for (FunctionValue(template, closures) <- functions.filter(!instantiated.contains(_))) {
                template.instantiate(closures, arguments, endOfBranchNode, returnMerger)
            }
        }
    }
}
