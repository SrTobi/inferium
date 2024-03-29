package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, DebugAdapter, ExecutionState}
import inferium.dataflow.graph.DebugNode._
import inferium.dataflow.graph.traits.{FailingTransformerNode, HeapReading, LexicalLookup}
import inferium.lattice._

class DebugNode(operations: Seq[Operation], lineNumber: Option[Int])(implicit _info: Node.Info) extends FailingTransformerNode with HeapReading with LexicalLookup {

    private val heapAccessLoc: Location = Location()

    private def report(f: (Node, String) => Unit)(msg: String)(implicit analysis: DataFlowAnalysis): Unit = f(this,  s"In line ${lineNumber.getOrElse("unknown")}: $msg")
    private def reportError(msg: String)(implicit analysis: DataFlowAnalysis): Unit = report(analysis.debugAdapter.error)(msg)
    private def reportWarning(msg: String)(implicit analysis: DataFlowAnalysis): Unit = report(analysis.debugAdapter.warn)(msg)
    private def reportInfo(msg: String)(implicit analysis: DataFlowAnalysis): Unit = analysis.debugAdapter.info(this, msg)

    override protected def transform(state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[ExecutionState] = {
        val stack = state.stack
        lazy val subject = if (stack.isEmpty)
                throw new IllegalStateException("Can't access ans element because it does not exist in the current context")
            else
                stack.head.normalized(state.heap.begin(heapAccessLoc))

        operations foreach {
            case CheckDeadCode =>
                reportError("Analyzing dead code!")

            case CheckLiveCode =>
                // nothing to do

            case Is(_) =>

            case PrintExpr(name) =>
                subject
                reportInfo(s"$name: $subject")
        }

        Some(state)
    }

    def executeChecks(debugAdapter: DebugAdapter)(implicit analysis: DataFlowAnalysis): Unit = {
        val state = inState
        lazy val heapAccess = state.heap.begin(heapAccessLoc)
        lazy val subject = state.stack.head.normalized(heapAccess)

        operations foreach {
            case CheckDeadCode =>
                // nothing to do

            case CheckLiveCode =>
                if (state == null) {
                    reportError("LiveCode check failed. There is reachable, but unanalyzed, code")
                    return
                }

            case Is(entites) =>
                if (state == null) {
                    return
                }
                val normalizedEntities = entites map {
                    case Left(p) => p
                    case Right(name) =>
                        val lookupChain = info.lexicalEnv.buildLookupSeq(name)
                        val (obj, propertyName, _) = lookup(state, lookupChain)
                        val Some((result, _)) = read(obj, StringLattice(propertyName), state)
                        result.normalized(heapAccess)
                }

                if (!(Entity.unify(normalizedEntities) == subject)) {
                    reportError(s"debugged expression [$subject] is not [${normalizedEntities.mkString(", ")}]")
                }

            case PrintExpr(_) =>
                // nothing to do
        }
    }

    override def asAsmStmt: String = s"debug [${operations.mkString(", ")}]"
}

object DebugNode {
    sealed abstract class Operation

    case object CheckDeadCode extends Operation {
        override def toString: String = "dead"
    }
    case object CheckLiveCode extends Operation {
        override def toString: String = "live"
    }
    case class Is(entities: Seq[Either[Entity, String]]) extends Operation {
        override def toString: String = s"is(${entities map { case Left(p) => p.toString case Right(s) => s} mkString ", "})"
    }
    case class PrintExpr(name: String) extends Operation {
        override def toString: String = "print"
    }
}