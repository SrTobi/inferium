package inferium.dataflow.graph

import inferium.dataflow.calls.CallInstance

import scala.collection.mutable
import inferium.dataflow.{DataFlowAnalysis, ExecutionState, LexicalEnv}
import inferium.lattice.{Entity, Location}
import inferium.utils.{Id, IdGenerator}

import scala.language.implicitConversions

abstract class Node(implicit val info: Node.Info) {
    import Node._

    protected implicit val thisOrigin: StateOrigin = StateOrigin()

    val id: NodeId = NodeId.newId()
    def label: String = info.label.getOrElse(s"L${id.id}")
    def catchTarget: Option[Node] = info.catchTarget

    def additionalInfos: Seq[AdditionalInfo] = Seq()

    var exprStackInfo: ExprStackInfo = _

    final def ~>(node: Node): Graph = {
        this.addSuccessor(node)
        node.addPredecessor(this)
        Graph(this, node)
    }

    final def ~>(graph: Graph): Graph = Graph(this) ~> graph

    final def ~/>(node: Node): Unit = {
        node.removePredecessor(this)
        this.removeSuccessor(node)
    }

    final def erase(): Unit = {
        predecessors foreach { _ ~/> this }
        successors foreach { this ~/> _ }
    }

    final def remove(): Unit = {
        val preds = predecessors
        val succs = successors

        erase()

        for (pred <- preds; succ <- succs) {
            pred ~> succ
        }
    }

    final def replace(nodes: Node*): Unit = {
        val preds = predecessors
        val succs = successors

        erase()

        for (pred <- preds; node <- nodes) {
            pred ~> node
        }

        for (succ <- succs; node <- nodes) {
            node ~> succ
        }
    }


    final def fail(state: ExecutionState, exception: Entity)(implicit analysis: DataFlowAnalysis): Unit = {
        Node.fail(info.catchTarget, state, exception)
    }

    final def fixLexicalFrame(state: ExecutionState): ExecutionState = state.copy(lexicalFrame = info.lexicalEnv.fixLexicalStack(state.lexicalFrame))

    /*final def fixState(state: ExecutionState): ExecutionState = {
        var frame = state.callFrame
        val targetDepth = info.callFrame.depth
        assert(frame.depth >= targetDepth)

        while (frame.depth > targetDepth) {
            frame = frame.next.get
        }

        val fixedLexicalFrame = info.lexicalEnv.fixLexicalStack(frame.lexicalFrame)
        val fixedCallFrame = frame.copy(lexicalFrame = fixedLexicalFrame)
        state.withCallFrame(fixedCallFrame)
    }*/

    def priority: Int = info.priority

    def hasPred: Boolean
    def hasSucc: Boolean

    def predecessors: Seq[Node]
    def successors: Seq[Node]

    protected[graph] def removePredecessor(node: Node): Unit
    protected[graph] def removeSuccessor(node: Node): Unit
    protected[graph] def addPredecessor(node: Node): Unit
    protected[graph] def addSuccessor(node: Node): Unit

    final def <~(state: ExecutionState)(implicit originNode: StateOrigin, analysis: DataFlowAnalysis): Unit = setNewInState(state, originNode)
    def setNewInState(state: ExecutionState, origin: NodeId)(implicit analysis: DataFlowAnalysis): Unit

    def process(implicit analysis: DataFlowAnalysis): Unit

    override def toString: String = s"[$id]$asAsmStmt"
    def asAsmStmt: String
}


object Node {
    type NodeId = Id[Node]
    object NodeId extends IdGenerator[Node]

    final class StateOrigin private (private val originId: Long) extends AnyVal {
        def id: NodeId = NodeId.createFromExisting(originId)
    }

    object StateOrigin {
        def apply(loc: NodeId = NodeId.newId()): StateOrigin = new StateOrigin(loc.id)
        implicit def stateOriginToId(origin: StateOrigin): NodeId = origin.id
    }

    sealed abstract class ExprStackFrame {
        //override def toString: String = super.toString
    }
    case class ExprStackFrameStringInfo(info: String) extends ExprStackFrame {
        override def toString: String = info
    }
    case class ExprStackFrameCombinationInfo(op: String, inner: Seq[ExprStackFrame]) extends ExprStackFrame {
        assert(inner.nonEmpty)

        override def toString: String = inner match {
            case Seq(one) => op + one
            case _ => inner.mkString("(", s" $op ", ")")
        }
    }

    object ExprStackFrame {
        def apply(info: String): ExprStackFrame = ExprStackFrameStringInfo(info)
        def apply(op: String, inner: Seq[ExprStackFrame]): ExprStackFrame = {
            val inners = inner flatMap {
                case ExprStackFrameCombinationInfo(innerOp, innerInners) if innerOp == op =>
                    innerInners
                case i =>
                    Seq(i)
            }
            ExprStackFrameCombinationInfo(op, inners)
        }
        def apply(op: String, inner: ExprStackFrame, restInner: ExprStackFrame*): ExprStackFrame = ExprStackFrameCombinationInfo(op, inner +: restInner)
    }

    type ExprStackInfo = List[ExprStackFrame]

    class CallFrame(val outer: Option[CallFrame], val callSite: Option[(AnyRef, CallInstance.RecursionAble)]) {
        assert(callSite.isDefined == outer.isDefined)
        val depth: Int = outer map { _.depth + 1 } getOrElse 0
        val callSites: Map[AnyRef, CallInstance.RecursionAble] = {
            val outerCallSites = outer.map { _.callSites } getOrElse Map.empty
            callSite map { outerCallSites + _ } getOrElse outerCallSites
        }

        def getRecursiveSite(anchor: AnyRef): Option[CallInstance.RecursionAble] = callSites.get(anchor)

        def ::(callSite: (AnyRef, CallInstance.RecursionAble)): CallFrame = new CallFrame(Some(this), Some(callSite))

        override def toString: String = s"CallFrame:$depth"
    }

    type CatchTarget = Option[MergeNode]

    def fail(catchTarget: CatchTarget, state: ExecutionState, exception: Entity)(implicit stateOrigin: StateOrigin, analysis: DataFlowAnalysis): Unit = {
        catchTarget foreach {
            _ <~ state.copy(stack = exception :: Nil)
        }
    }

    case class Info(priority: Int, lexicalEnv: LexicalEnv, callFrame: CallFrame, catchTarget: CatchTarget, label: Option[String] = None)

    case class AdditionalInfo(key: String, name: String)

    def isForwardEdge(from: Node, to: Node): Boolean = {
        to match {
            case to: MergeNode if to.isFixpoint =>
                from.info.priority <= to.info.priority
            case _ =>
                true
        }
    }

    def isBackwardsEdge(from: Node, to: Node): Boolean = !isForwardEdge(from, to)

    trait Visitor {
        def start(startNode: Node): this.type
    }

    abstract class AllVisitor extends Visitor {
        private val visited = mutable.Set.empty[Node]
        private val nodesToVisit = mutable.PriorityQueue.empty[Node](Ordering.by(_.priority))

        protected def stop(): this.type = {
            nodesToVisit.clear()
            this
        }

        def start(graph: Graph): this.type = graph match {
            case Graph(begin, end, priority) => start(begin, Some(end), priority)
            case _ => this
        }
        def start(startNode: Node): this.type = start(startNode, None, startNode.priority)
        def start(startNode: Node, endNode: Node): this.type = start(startNode, Some(endNode), Math.min(startNode.priority, endNode.priority))
        def start(startNode: Node, endNode: Option[Node], minPriority: Int): this.type = {
            assert(nodesToVisit.isEmpty)

            var nextNode:Option[Node] = Some(startNode)

            while (nodesToVisit.nonEmpty || nextNode.nonEmpty) {
                val cur = nextNode.getOrElse(nodesToVisit.dequeue())
                nextNode = None
                if (!visited(cur)) {
                    //println(cur.priority + " -> " + cur)
                    visited += cur
                    cur.successors.filter(_.priority >= minPriority) match {
                        case Seq(first) if first.priority == cur.priority =>
                            nextNode = Some(first)
                        case all =>
                            nodesToVisit ++= all
                    }
                    visit(cur)
                }
            }
            this
        }

        protected def visit(node: Node): Unit
    }
}