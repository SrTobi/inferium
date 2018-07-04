package inferium.dataflow.graph.traits

import inferium.dataflow.ExecutionState
import inferium.lattice.{Entity, Location}


abstract class SeqSpreader {
    def spread(seq: Seq[Entity], state: ExecutionState): (Seq[Entity], Option[Entity], ExecutionState)
}

object SeqSpreader {
    private case class SeqSpreaderImpl(spreads: Seq[Boolean]) extends SeqSpreader {
        assert(spreads contains true)

        private val accessLocations = Stream.continually(Location())

        def spread(seq: Seq[Entity], state: ExecutionState): (Seq[Entity], Option[Entity], ExecutionState) = {
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
            (result, rest, state)
        }
    }

    private object InactiveSeqSpreader extends SeqSpreader {
        override def spread(seq: Seq[Entity], state: ExecutionState): (Seq[Entity], Option[Entity], ExecutionState) = (seq, None, state)
    }

    def apply(spreads: Seq[Boolean]): SeqSpreader = if (spreads contains true) SeqSpreaderImpl(spreads) else InactiveSeqSpreader
}