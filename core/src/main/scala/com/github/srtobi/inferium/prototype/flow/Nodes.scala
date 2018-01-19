package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.Ast
import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.annotation.tailrec
import scala.collection.mutable

abstract class ValueSourceProvider {
    def newSource(): ValueSource
}

class ValueSink extends ValueSourceProvider {
    private var currentValue: ValueLike = NeverValue

    def set(value: ValueLike): Unit = {
        assert(value ne null)
        currentValue = value
    }

    override def newSource(): ValueSource = new ValueSource {
        override def get(): ValueLike = {
            assert(currentValue ne null)
            return currentValue
        }
    }
}

object ValueSink {
    def wrap(value: ValueLike): ValueSourceProvider = {
        val sink = new ValueSink
        sink.set(value)
        return sink
    }
}

abstract class ValueSource {
    def get(): ValueLike
}

object ValueSource {
    def wrap(value: ValueLike): ValueSource = ValueSink.wrap(value).newSource()
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
                val realTargets = heap.filterUnwritables(targetValue)
                val value = heap.readProperty(realTargets, propertyName)
                _result.set(Reference(value, targetValue, propertyName))
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
    class PropertyWrite(val target: ValueSource, val propertyName: String, val value: ValueSource)(implicit flowAnalysis: FlowAnalysis) extends Node {

        override def onControlFlow(heap: HeapMemory): Unit = {
            val targetValue = target.get()
            if (!targetValue.throwsWhenWrittenOrReadOn) {
                val valueHandle = value.get()
                val realTargets = heap.filterUnwritables(targetValue)
                heap.writeProperty(realTargets, propertyName, valueHandle)
                controlFlowTo(next, heap)
            } else {
                // program stops
                noControlFlowTo(next)
            }
        }
    }

    /*
     * - left/right have an upper bound on number
     * - the result has a lower bound on number or is the result of the subtraction of left and right
     */
    class Subtraction(val left: ValueSource, val right: ValueSource)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new ValueSink
        def result: ValueSourceProvider = _result

        override def onControlFlow(heap: HeapMemory): Unit = {
            lazy val leftValue = left.get().asValue
            lazy val rightValue = right.get().asValue

            (leftValue, rightValue) match {
                case (SpecificNumberValue(leftNum), SpecificNumberValue(rightNum)) =>
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
    class Literal[T <: ValueLike](val literal: T)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private val _result = new ValueSink
        def result: ValueSourceProvider = _result

        override def onControlFlow(heap: HeapMemory): Unit = {
            _result.set(literal)

            controlFlowTo(next, heap)
        }
    }

    class NewObject()(implicit flowAnalysis: FlowAnalysis) extends Literal[ObjectValue](flowAnalysis.solver.newEmptyObject()) {
        override def onControlFlow(heap: HeapMemory): Unit = {
            heap.createObject(literal)
            super.onControlFlow(heap)
        }
    }

    class Conditional(val cond: ValueSource, val thenBranch: (Node, Node), val elseBranch: Option[(Node, Node)])(implicit flowAnalysis: FlowAnalysis) extends Node {

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
                    controlFlowTo(thenBegin, truthyfy(condValue, heap.split()))
                    controlFlowTo(elseBegin, falsyfy(condValue, heap.split()))

                case BoolLattice.True =>
                    controlFlowTo(thenBegin, truthyfy(condValue, heap))
                    noControlFlowTo(elseBegin)

                case BoolLattice.False =>
                    controlFlowTo(elseBegin, falsyfy(condValue, heap))
                    noControlFlowTo(thenBegin)
            }
        }

        private def truthyfy(cond: ValueLike, heap: HeapMemory): HeapMemory = {
            cond.truthy(heap)
            heap
        }

        private def falsyfy(cond: ValueLike, heap: HeapMemory): HeapMemory = {
            cond.falsy(heap)
            heap
        }

        private def connectBranch(branch: (Node, Node)): Unit = branch match {
            case (begin, end) =>
                assert((end.next eq endOfBranchNode) || (end.next eq null))
                if (end.next eq null) {
                    end.next = endOfBranchNode
                }
        }
    }

    class FunctionCall(val target: ValueSource, val arguments: Seq[ValueSourceProvider], val callStack: Templates.CallStack)(implicit flowAnalysis: FlowAnalysis) extends Node {
        private type CallContext = (Nodes.Node, Seq[ValueSourceProvider])
        private val calls = mutable.Map.empty[FunctionValue, CallContext]
        private val _result = new ValueSink
        private val allReturns = mutable.Buffer.empty[ValueSource]
        def result: ValueSourceProvider = _result

        // recursion stuff
        private var hasRecursion: Boolean = _
        private var currentReturn: ValueLike = NeverValue
        private var invariantHeap: HeapMemory = _
        private var currentArguments: Option[Seq[ValueLike]] = None
        private var mergedArguments: Seq[ValueSink] = arguments.map(_ => new ValueSink)
        private var argsChanged = false

        private val mergeNode = new MergeNode(0, new Node() {

            override def onControlFlow(heap: HeapMemory): Unit = {
                val ret = UnionValue(allReturns.map(_.get()): _*)
                _result.set(ret)

                val retValue = ret.normalized

                val heapAfterCall = if (hasRecursion) {
                    val mergedHeap = flowAnalysis.unify(invariantHeap, heap)
                    if (argsChanged || !mergedHeap.squashed().structureEquals(invariantHeap.squashed()) || !retValue.structureEquals(currentReturn)) {
                        // invariant heap has changed... go again
                        currentReturn = retValue
                        controlFlowTo(FunctionCall.this, mergedHeap)
                        return
                    }
                    mergedHeap
                } else {
                    heap
                }

                currentArguments = None
                invariantHeap = null
                if (retValue == NeverValue) {
                    noControlFlowTo(FunctionCall.this.next)
                } else {
                    controlFlowTo(FunctionCall.this.next, heapAfterCall)
                }
            }

            override def onNoControlFlow(): Unit = {
                invariantHeap = null
                noControlFlowTo(FunctionCall.this.next)
            }
        })

        override def onControlFlow(heap: HeapMemory): Unit = {
            val funcVal = target.get().remove(heap, !_.asValue.isInstanceOf[FunctionLike])
            val functions = funcVal.asFunctions

            hasRecursion = false
            argsChanged = false
            invariantHeap = heap.split()

            val args = currentArguments match {
                case Some(values) => values
                case None =>
                    val values = arguments.map(_.newSource().get())
                    currentArguments = Some(values)
                    values
            }

            // set arguments
            mergedArguments.zip(args).foreach {
                case (sink, arg) => sink.set(arg)
            }

            allReturns.clear()
            lazy val argumentValues = mergedArguments.map(_.newSource().get())

            val begins = functions.map {
                case uv: UserValue =>
                    allReturns += ValueSource.wrap(uv.onCall(argumentValues))
                    mergeNode
                case func@FunctionValue(template, closures) =>
                    callStack.get(template) match {
                        case Some(prevCallNode) =>
                            prevCallNode.hasRecursion = true
                            prevCallNode.currentArguments = Some(prevCallNode.currentArguments.get.zipAll(argumentValues, UndefinedValue, UndefinedValue).map {
                                case (old, a) =>
                                    val merged = UnionValue(old, a).normalized
                                    if (!merged.structureEquals(old.normalized)) {
                                        prevCallNode.argsChanged = true
                                    }
                                    merged
                            })
                            allReturns += ValueSource.wrap(prevCallNode.currentReturn)
                            mergeNode
                        case None =>
                            val (begin, returns) = calls.getOrElseUpdate(func, template.instantiate(closures, mergedArguments, callStack + (template -> this), mergeNode))
                            allReturns ++= returns.map(_.newSource())
                            begin
                    }
            }

            mergeNode.setNumBranchesToWaitFor(allReturns.length)


            if (begins.isEmpty) {
                noControlFlowTo(next)
            } else {
                for (begin <- begins) {
                    controlFlowTo(begin, heap.split())
                }
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
