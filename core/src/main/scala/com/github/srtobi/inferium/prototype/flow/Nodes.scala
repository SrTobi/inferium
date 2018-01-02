package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.annotation.tailrec
import scala.collection.mutable

abstract class ValueSourceProvider {
    def newSource(): ValueSource
}

class ValueSink extends ValueSourceProvider {
    private var currentValue: Value = NeverValue

    def set(value: Value): Unit = {
        assert(value ne null)
        currentValue = value
    }

    override def newSource(): ValueSource = new ValueSource {
        override def get(): Value = {
            assert(currentValue ne null)
            return currentValue
        }
    }
}
abstract class ValueSource {
    def get(): Value
}



object Nodes {

    abstract class Node()(implicit val flowAnalysis: FlowAnalysis) {
        private var nextNode: Node = _

        def next: Node = nextNode
        def next_=(node: Node): Unit = {
            assert(nextNode == null)
            nextNode = node
        }

        def solver: Solver = flowAnalysis.solver

        /*protected final def activate(node: Node, heapState: State): Unit = flowAnalysis.activate(node, heapState)
        protected final def activate(node: Node, heapStateBuilder: HeapStateBuilder): Unit = activate(node, heapStateBuilder.end())

        def doActivation(inHeap: State): Unit = {
            if (activated)
                return
            activated = true
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

        // when the value properties change
        def onValueChange(): Unit = {}*/

        final def controlFlowTo(next: Node, heap: HeapMemory): Unit = flowAnalysis.controlFlowTo(next, heap)
        final def noControlFlowTo(next: Node): Unit = flowAnalysis.noControlFlowTo(next)

        def onControlFlow(heap: HeapMemory): Unit
        def onNoControlFlow(): Unit = noControlFlowTo(next)
    }

    class BeginNode(implicit flowAnalysis: FlowAnalysis) extends Node {
        override def onControlFlow(heap: HeapMemory): Unit = {
            controlFlowTo(next, heap)
        }
    }

    class MergeNode(private var numBranchesToWaitFor: Int, nextNode: Node)(implicit flowAnalysis: FlowAnalysis) extends Node {

        private var waitingHeap: HeapMemory = _

        assert(numBranchesToWaitFor >= 0)
        this.next = nextNode

        def setNumBranchesToWaitFor(num: Int): Unit = {
            assert(numBranchesToWaitFor == 0)
            numBranchesToWaitFor = num
        }

        private def pushHeap(heap: HeapMemory): Unit = {
            if (waitingHeap eq null) {
                waitingHeap = heap
            } else {
                waitingHeap = flowAnalysis.unify(waitingHeap, heap)
            }
        }

        override def onControlFlow(heap: HeapMemory): Unit = {
            assert(numBranchesToWaitFor > 0)
            numBranchesToWaitFor -= 1
            pushHeap(heap)
            if (numBranchesToWaitFor == 0) {
                controlFlowTo(next, waitingHeap)
                waitingHeap = null
            }
        }

        override def onNoControlFlow(): Unit = {
            assert(numBranchesToWaitFor > 0)
            numBranchesToWaitFor -= 1
            if (numBranchesToWaitFor == 0) {
                if (waitingHeap ne null) {
                    controlFlowTo(next, waitingHeap)
                    waitingHeap = null
                } else {
                    noControlFlowTo(next)
                }
            }
        }
    }


    /*
     * - result has a lower bound on Val while target = { propertyName: (Val,) }
     * - target has an upper bound on { propertyName: (,result) }
     * - all subsequent reads to propertyName on target must yield the same Value
     */
    class PropertyRead(val target: ValueSource, val propertyName: String)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new ValueSink
        def result: ValueSourceProvider = _result

        override def onControlFlow(heap: HeapMemory): Unit = {
            val targetValue = target.get()
            if (!targetValue.throwsWhenWrittenOrReadOn) {
                _result.set(heap.readProperty(targetValue, propertyName))
                controlFlowTo(next, heap)
            } else {
                // program stops
                noControlFlowTo(next)
            }
        }
        /*override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            targetReader = heapFlow.newHandleReader(target, this)
            merger = heapFlow.newHandleMerge()
        }

        override def onHandleChanged(oldValue: Option[Value], newValue: Value): Unit = {
            // writing on null or undefined ends the program
            if (!newValue.throwsWhenWrittenOrReadOn) {
                activate(next, outHeap)
            }
        }

        override def onValueChange(): Unit = {
            val target = targetReader.read()
            merger.add(target.getProperty(propertyName).toSeq: _*)
        }*/
    }


    /*
     * - result has a lower bound on Val while target = { propertyName: (Val,) }
     * - target has an upper bound on { propertyName: (,result) }
     * - all subsequent reads to propertyName on target must yield the same Value
     */
    /*class LocalRead(val target: EmptyObject, val propertyName: String)(implicit flowAnalysis: FlowAnalysis) extends Node {
        val result: ValueHandle = heap.newValueHandle()

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            heapFlow.readLocal(target, propertyName, result)
        }
    }*/

    /*
     * - target has an upper bound on { propertyName: value }
     * - value has an upper bound on { propertyName: ... }
     */
    class PropertyWrite(val target: ValueSource, val propertyName: String, val value: ValueSource)(implicit flowAnalysis: FlowAnalysis) extends Node {

        /*override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            targetReader = heapFlow.newHandleReader(target, this)
            targetWriter = heapFlow.newHandleWriter(target)
        }

        override def onHandleChanged(oldValue: Option[Value], newValue: Value): Unit = {
            // writing on null or undefined ends the program
            if (!newValue.throwsWhenWrittenOrReadOn) {
                activate(next, outHeap)
                targetWriter.write(solver.withProperty(newValue, propertyName, value))
            }
        }*/
        override def onControlFlow(heap: HeapMemory): Unit = {
            val targetValue = target.get()
            if (!targetValue.throwsWhenWrittenOrReadOn) {
                val valueHandle = value.get()
                heap.writeProperty(targetValue, propertyName, valueHandle)
                controlFlowTo(next, heap)
            } else {
                // program stops
                noControlFlowTo(next)
            }
        }
    }

    /*
     * - target has an upper bound on { propertyName: value }
     * - value has an upper bound on { propertyName: ... }
     */
    /*class LocalWrite(val target: EmptyObject, val propertyName: String, val value: ValueHandle)(implicit flowAnalysis: FlowAnalysis) extends Node {

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            heapFlow.writeLocal(target, propertyName, value)
        }
    }*/

    /*
     * - left/right have an upper bound on number
     * - the result has a lower bound on number or is the result of the subtraction of left and right
     */
    class Subtraction(val left: ValueSource, val right: ValueSource)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new ValueSink
        def result: ValueSourceProvider = _result

        /*private def leftValue = leftReader.read()
        private def rightValue = rightReader.read()

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            leftReader = heapFlow.newHandleReader(left, this)
            rightReader = heapFlow.newHandleReader(right, this)
            resultWriter = heapFlow.newHandleWriter(result)
            outHeap = heapFlow.end()
            activate(next, outHeap)
        }

        override def onHandleChanged(oldValue: Option[Value], newValue: Value): Unit = {
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
        }*/

        override def onControlFlow(heap: HeapMemory): Unit = {
            lazy val leftValue = left.get()
            lazy val rightValue = right.get()

            (leftValue, rightValue) match {
                case (NumberValue(leftNum), NumberValue(rightNum)) =>
                    _result.set(solver.number((leftNum - rightNum).toString))
                case _ =>
                    _result.set(solver.number())
            }
            controlFlowTo(next, heap)
        }
    }

    /*
     * - the result has a lower bound on value
     */
    class Literal(val literal: Value)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new ValueSink
        def result: ValueSourceProvider = _result

        override def onControlFlow(heap: HeapMemory): Unit = {
            _result.set(literal)

            controlFlowTo(next, heap)
        }
        /*override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            resultWriter = heapFlow.newHandleWriter(result)
            resultWriter.write(literal)
            outHeap = heapFlow.end()
            activate(next, outHeap)
        }*/
    }

    class NewObject()(implicit flowAnalysis: FlowAnalysis) extends Literal(flowAnalysis.solver.newEmptyObject())

    class Conditional(val cond: ValueSource, val thenBranch: (Node, Node), val elseBranch: Option[(Node, Node)])(implicit flowAnalysis: FlowAnalysis) extends Node {

        /*private var thenConnected: Boolean = false
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
            condReader = heapFlow.newHandleReader(cond, this)
            heapAfterCond = heapFlow.end()
        }

        override def onHandleChanged(oldValue: Option[Value], newValue: Value): Unit = {
            // this will also call onValueChange
            // nothing to do here
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
        }*/

        lazy val endOfBranchNode = new MergeNode(0, next)
        override def onControlFlow(heap: HeapMemory): Unit = {
            val condValue = cond.get()
            endOfBranchNode.setNumBranchesToWaitFor(2)

            connectBranch(thenBranch)
            elseBranch.foreach(connectBranch)

            val thenBegin = thenBranch._1
            val elseBegin = elseBranch.map(_._1).getOrElse(endOfBranchNode)

            condValue.asBool match {
                case BoolLattice.Top =>
                    controlFlowTo(thenBegin, heap)
                    controlFlowTo(elseBegin, heap)

                case BoolLattice.True =>
                    controlFlowTo(thenBegin, heap)
                    noControlFlowTo(elseBegin)

                case BoolLattice.False =>
                    controlFlowTo(elseBegin, heap)
                    noControlFlowTo(thenBegin)
            }
        }

        private def connectBranch(branch: (Node, Node)): Unit = branch match {
            case (begin, end) =>
                assert((end.next eq endOfBranchNode) || (end.next eq null))
                if (end.next eq null) {
                    end.next = endOfBranchNode
                }
        }
    }

    class FunctionCall(val target: ValueSource, val arguments: Seq[ValueSourceProvider])(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new ValueSink
        def result: ValueSourceProvider = _result
        /*private var targetReader: HandleReader = _
        private val instantiated = mutable.Set.empty[FunctionValue]
        private val endOfBranchNode = new Node {
            override def onActivate(heapFlow: HeapStateBuilder): Unit = {
                activate(FunctionCall.this.next, FunctionCall.this.mergeHeap)
            }
        }

        private val mergeHeap = heap.newMergeHeapState()
        private val returnMerger = {
            val (rm, outH) = mergeHeap.newValueHandleMerger()
            outHeap = outH
            rm
        }
        def result: ValueHandle = returnMerger

        override def onActivate(heapFlow: HeapStateBuilder): Unit = {
            targetReader = heapFlow.newHandleReader(target, this)
        }

        override def onHandleChanged(oldValue: Option[Value], newValue: Value): Unit = {
            // do nothing
        }

        override def onValueChanged(): Unit = {
            val functions = targetReader.read().asFunctions

            // TODO: use effects

            for (FunctionValue(template, closures) <- functions.filter(!instantiated.contains(_))) {
                template.instantiate(closures, arguments, endOfBranchNode, returnMerger)
            }
        }*/


        override def onControlFlow(heap: HeapMemory): Unit = {
            val funcVal = target.get()
            val functions = funcVal.asFunctions

            val allReturns = mutable.Buffer.empty[ValueSource]
            val mergeNode = new MergeNode(0, new Node() {
                override def onControlFlow(heap: HeapMemory): Unit = {
                    _result.set(UnionValue(allReturns.map(_.get()): _*))
                    controlFlowTo(FunctionCall.this.next, heap)
                }

                override def onNoControlFlow(): Unit = noControlFlowTo(FunctionCall.this.next)
            })

            val begins = for (FunctionValue(template, closures) <- functions) yield {
                val (begin, returns) = template.instantiate(closures, arguments, mergeNode)
                allReturns ++= returns.map(_.newSource())
                begin
            }

            mergeNode.setNumBranchesToWaitFor(allReturns.length)

            for (begin <- begins) {
                controlFlowTo(begin, heap.split())
            }
        }
    }

    class ReturnNode(val returnValue: ValueSource, val deadNode: Option[Node])(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new ValueSink
        def result: ValueSourceProvider = _result
        override def onControlFlow(heap: HeapMemory): Unit = {
            _result.set(returnValue.get())
            deadNode.foreach(noControlFlowTo)
            controlFlowTo(next, heap)
        }

        override def onNoControlFlow(): Unit = {
            _result.set(NeverValue)
            deadNode.foreach(noControlFlowTo)
            noControlFlowTo(next)
        }
    }
}
