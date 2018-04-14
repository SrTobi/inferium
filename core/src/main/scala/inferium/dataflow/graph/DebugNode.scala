package inferium.dataflow.graph
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.DebugNode.{CheckDeadCode, CheckLiveCode, OneOf, Operation}
import inferium.lattice.{Primitive, ValueLocation}

class DebugNode(operations: Seq[Operation])(implicit _info: Node.Info) extends FailingTransformerNode with HeapReading with LexicalLookup {
    override protected def transform(state: ExecutionState, analysis: DataFlowAnalysis): Option[ExecutionState] = {
        lazy val subject = state.stack.head
        val error = analysis.debugAdapter.error(this, _: String)

        operations foreach {
            case CheckDeadCode =>
                error("Analyzing dead code!")

            case CheckLiveCode =>
                // nothing to do

            case OneOf(entites) =>
                val normalizedEntities = entites map {
                    case Left(p) => p
                    case Right(name) =>
                        val lookupChain = info.lexicalEnv.buildLookupSeq(name)
                        val (obj, propertyName, _) = lookup(state, lookupChain)
                        val Some((result, _)) = read(Seq(obj), ValueLocation.Scope, propertyName, state)
                        result
                }

                if (!normalizedEntities.exists(subject.mightBe)) {
                    error(s"debugged expression was none of [${normalizedEntities.mkString(", ")}]")
                }
        }

        Some(state)
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
    case class OneOf(entities: Seq[Either[Primitive, String]]) extends Operation {
        override def toString: String = s"oneOf(${entities map { case Left(p) => p.toString case Right(s) => s} mkString ", "})"
    }

}