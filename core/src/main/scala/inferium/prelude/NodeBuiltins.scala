package inferium.prelude

import inferium.dataflow.DataFlowAnalysis
import inferium.dataflow.calls.{NativeCall, NativeCallableInfo}
import inferium.lattice.StringLattice.SpecificStrings
import inferium.lattice._

object NodeBuiltins {
    val require: NativeCallableInfo = NativeCall.createSimpleCallableInfo("require",
        (heap: Heap, thisEntity: Entity, args: Seq[Entity], rest: Entity, analysis: DataFlowAnalysis) => {
            val arg = args.headOption getOrElse rest
            val mutator = heap.begin(Location())
            val result = arg.asStringLattice(mutator) match {
                case SpecificStrings(strings) if strings.nonEmpty =>
                    Entity.unify(strings.iterator map { analysis.requireModule(_).getOrElse(AnyEntity) })
                case _ => AnyEntity
            }
            (heap.end(mutator), result)
        })
}
