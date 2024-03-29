package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice._

trait HeapReading extends Node {
    private val heapResolveLoc = Location()
    private val heapReadingLoc = Location()
    private lazy val readProbe = new ProbeEntity

    protected final def read(target: Entity, properties: StringLattice, state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[(Entity, ExecutionState)] = {
        val initialHeap = state.heap

        val resolveMutator = initialHeap.begin(heapResolveLoc)
        val objs = target.coerceToObjects(resolveMutator)
        val probes = target.asProbes(resolveMutator)
        val heapAfterCoersion = initialHeap.end(resolveMutator)

        if (objs.isEmpty && probes.isEmpty) {
            // todo: generate exception object
            fail(state.copy(heap = heapAfterCoersion), UndefinedValue)
            return None
        }
        val readMutator = heapAfterCoersion.begin(heapReadingLoc)

        def dynReadsFromObject(onlyNumbers: Boolean): Entity = UnionValue(
            for (obj <- objs) yield {
                dynRead(target, obj, onlyNumbers, readMutator)
            }
        )


        val result = properties match {
            case StringLattice.Top =>
                probes foreach { _.dynRead(readProbe) }
                dynReadsFromObject(false)
            case StringLattice.NumberString =>
                probes foreach { _.numberRead(readProbe) }
                dynReadsFromObject(true)
            case StringLattice.SpecificStrings(propertyNames) =>
                for (probe <- probes; propertyName <- propertyNames)
                    probe.read(propertyName, readProbe)
                UnionValue(
                    for (obj <- objs; propertyName <- propertyNames)
                        yield {
                            read(target, obj, propertyName, readMutator)
                        }
                )
        }

        val resultHeap = heapAfterCoersion.end(readMutator)

        val resultState = state.copy(heap = resultHeap)
        Some((if (probes.isEmpty) result else result unify readProbe, resultState))
    }

    private final def dynRead(base: Entity, obj: ObjectLike, numbersOnly: Boolean, mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Entity = UnionValue(
        mutator.getProperties(obj, numbersOnly).map {
            case (name, p) => processProperty(base, name, p, mutator)
        }
    ) unify UndefinedValue // todo: add withUndefined

    private final def read(base: Entity, obj: ObjectLike, propertyName: String, mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Entity = {

        val property = mutator.getProperty(obj, propertyName)
        processProperty(base, Some(propertyName), property, mutator)
    }

    private final def processProperty(base: Entity, name: Option[String], property: Property, mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Entity = property match {
        case ConcreteProperty(_, _, target, _, getter, _) =>
            if (getter.nonEmpty) {
                // todo: implement getters
                ???
            }

            name match {
                case Some(propertyName) =>
                    Ref(base, propertyName, target)
                case None =>
                    UnionValue(target map { mutator.getValue })
            }

        case AbstractProperty(_, _, value, _, getter, _, mightBeAbsent) =>
            if (getter != NeverValue && getter != AnyEntity) {
                // todo: implement getters
                ???
            }
            if (mightBeAbsent) UndefinedValue | value else value
    }
}
