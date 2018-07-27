package inferium.prelude

import inferium.dataflow.DataFlowAnalysis
import inferium.dataflow.calls.{NativeCall, NativeCallableInfo}
import inferium.lattice.StringLattice.SpecificStrings
import inferium.lattice._

object NodeBuiltins {
    val require: NativeCallableInfo = NativeCall.createSimpleCallableInfo("require",
        (heap: Heap, thisEntity: Entity, args: Seq[Entity], rest: Entity, analysis: DataFlowAnalysis) => {
            val arg = args.headOption getOrElse rest
            var moduleNames = arg.asStringLattice(heap.begin(Location()))
            var h = heap
            val result = moduleNames match {
                case SpecificStrings(strings) if strings.nonEmpty =>
                    Entity.unify(strings.toSeq map {
                        moduleName =>
                            val (module, newHeap) = analysis.requireModule(moduleName, h).getOrElse(AnyEntity -> h)
                            h = newHeap
                            module
                    })

                case _ => AnyEntity
            }
            (h, result)
        })
}
