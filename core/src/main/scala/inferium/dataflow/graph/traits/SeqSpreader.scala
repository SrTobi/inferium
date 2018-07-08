package inferium.dataflow.graph.traits

import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice.{Entity, Location}


abstract class SeqSpreader(spreads: Seq[Boolean]) extends Async[(Seq[Entity], Option[Entity])] {
    def spread(seq: Seq[Entity], state: ExecutionState)(implicit dataFlowAnalysis: DataFlowAnalysis): Unit = {
        assert(seq.length <= spreads.length)
        var rest: Option[Entity] = None

        val result = seq zip spreads flatMap {
            case (arg, isSpread) =>
                if (isSpread) {
                    ???
                } else {
                    Seq(arg)
                }
        }

        complete((result, rest), state, dataFlowAnalysis)
    }
}
