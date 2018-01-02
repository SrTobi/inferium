package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.annotation.tailrec
import scala.collection.mutable

abstract class HandleSourceProvider {
    def newSource(): HandleSource
}

class HandleSink extends HandleSourceProvider {
    private var currentHandle: HeapHandle = _

    def set(handle: HeapHandle): Unit = currentHandle = handle

    override def newSource(): HandleSource = new HandleSource {
        override def hasChanged: Boolean = true
        override def get(): HeapHandle = {
            assert(currentHandle ne null)
            return currentHandle
        }
        override def noControlGet(): Option[HeapHandle] = {
            return Option(currentHandle)
        }
    }
}
abstract class HandleSource {
    def hasChanged: Boolean
    def get(): HeapHandle
    def noControlGet(): Option[HeapHandle]
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

        assert(numBranchesToWaitFor > 0 || numBranchesToWaitFor == -1)
        this.next = nextNode

        def setNumBranchesToWaitFor(num: Int): Unit = {
            assert(numBranchesToWaitFor == -1)
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
    class PropertyRead(val target: HandleSource, val propertyName: String)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new HandleSink
        def result: HandleSourceProvider = _result

        override def onControlFlow(heap: HeapMemory): Unit = {
            _result.set(heap.readProperty(target.get(), propertyName))
            controlFlowTo(next, heap)
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
    class PropertyWrite(val target: HandleSource, val propertyName: String, val value: HandleSource)(implicit flowAnalysis: FlowAnalysis) extends Node {

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
            val targetHandle = target.get()
            val targetValue = heap.read(targetHandle)
            if (!targetValue.throwsWhenWrittenOrReadOn) {
                val valueHandle = value.get()
                heap.writeProperty(targetHandle, propertyName, valueHandle)
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
    class Subtraction(val left: HandleSource, val right: HandleSource)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new HandleSink
        def result: HandleSourceProvider = _result

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

        private val resultHandle = flowAnalysis.newHeapHandle()

        override def onControlFlow(heap: HeapMemory): Unit = {
            lazy val leftValue = heap.read(left.get())
            lazy val rightValue = heap.read(right.get())

            (leftValue, rightValue) match {
                case (NumberValue(leftNum), NumberValue(rightNum)) =>
                    heap.write(resultHandle, solver.number((leftNum + rightNum).toString))
                case _ =>
                    heap.write(resultHandle, solver.number())
            }
            _result.set(resultHandle)
            controlFlowTo(next, heap)
        }
    }

    /*
     * - the result has a lower bound on value
     */
    class Literal(val literal: Value)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val resultHandle = flowAnalysis.newHeapHandle()
        private val _result = new HandleSink
        def result: HandleSourceProvider = _result

        override def onControlFlow(heap: HeapMemory): Unit = {
            heap.write(resultHandle, literal)
            _result.set(resultHandle)

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

    class Conditional(val cond: HandleSource, val thenBranch: (Node, Node), val elseBranch: Option[(Node, Node)])(implicit flowAnalysis: FlowAnalysis) extends Node {

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

        override def onControlFlow(heap: HeapMemory): Unit = {
            val endOfBranchNode = new MergeNode(2, next)
            val condHandle = cond.get()
            val condValue = heap.read(condHandle)

            condValue.asBool match {
                case BoolLattice.Top =>
                    connectThenBranch(heap, endOfBranchNode)
                    connectElseBranch(heap, endOfBranchNode)

                case BoolLattice.True =>
                    noControlFlowTo(elseBranch.map(_._1).getOrElse(endOfBranchNode))
                    connectThenBranch(heap, endOfBranchNode)

                case BoolLattice.False =>
                    noControlFlowTo(thenBranch._1)
                    connectThenBranch(heap, endOfBranchNode)
            }
        }

        private def connectThenBranch(heap: HeapMemory, endOfBranchNode: Node): Unit = connectBranch(thenBranch, heap, endOfBranchNode)
        private def connectElseBranch(heap: HeapMemory, endOfBranchNode: Node): Unit = {
            elseBranch match {
                case Some(elseBra) =>
                    connectBranch(elseBra, heap, endOfBranchNode)
                case None =>
                    controlFlowTo(endOfBranchNode, heap)
            }
        }

        private def connectBranch(branch: (Node, Node), heap: HeapMemory, endOfBranchNode: Node): Unit = branch match {
            case (begin, end) =>
                assert((end.next eq endOfBranchNode) || (end.next eq null))
                if (end.next eq null) {
                    end.next = endOfBranchNode
                }
                controlFlowTo(begin, heap)
        }
    }

    class FunctionCall(val target: HandleSource, val arguments: Seq[HandleSourceProvider])(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new HandleSink
        def result: HandleSourceProvider = _result
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
            val funcVal = heap.read(target.get())
            val functions = funcVal.asFunctions

            val allReturns = mutable.Buffer.empty[HandleSource]
            val mergeNode = new MergeNode(-1, new Node() {
                override def onControlFlow(heap: HeapMemory): Unit = {
                    _result.set(heap.unifyHandles(allReturns.flatMap(_.noControlGet())))
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

    class ReturnNode(val deadNode: Node)(implicit flowAnalysis: FlowAnalysis) extends Node {
        override def onControlFlow(heap: HeapMemory): Unit = {
            noControlFlowTo(deadNode)
            controlFlowTo(next, heap)
        }
    }
}
