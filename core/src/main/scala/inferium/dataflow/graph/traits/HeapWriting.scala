package inferium.dataflow.graph.traits

import inferium.dataflow.graph.Node
import inferium.dataflow.{DataFlowAnalysis, ExecutionState}
import inferium.lattice._

trait HeapWriting extends Node {
    private val heapWritingLoc = Location()
    private val heapResolveLoc = Location()
    private val valueLocation = Location()

    protected final def write(target: Entity, properties: StringLattice, value: Entity, state: ExecutionState)(implicit analysis: DataFlowAnalysis): Option[(Entity, ExecutionState)] = {
        val initialHeap = state.heap

        val resolveMutator = initialHeap.begin(heapResolveLoc)
        val objs = target.coerceToObjects(resolveMutator)
        val heapAfterCoersion = initialHeap.end(resolveMutator)

        if (objs == Seq()) {
            // todo: generate exception object
            fail(state.copy(heap = heapAfterCoersion), UndefinedValue)
            return None
        }

        val writeMutator = heapAfterCoersion.begin(heapWritingLoc)
        var assignmentLocation = writeMutator.setValue(valueLocation, value)

        def dynWritesToObject(onlyNumbers: Boolean): Unit = {
            for (obj <- objs) {
                dynWrite(target, obj, assignmentLocation, value, onlyNumbers, writeMutator)
            }
        }
        val result = properties match {
            case StringLattice.AnyString =>
                dynWritesToObject(false)
                value

            case StringLattice.NumberString =>
                dynWritesToObject(true)
                value

            case StringLattice.SpecificStrings(propertyNames) =>

                val isCertainWrite = objs.tail.isEmpty && propertyNames.size == 1
                var valueLocationWasWritten = false
                for (obj <- objs; propertyName <- propertyNames) {
                    val res = write(target, obj, propertyName, assignmentLocation, value, isCertainWrite, writeMutator)
                    if (res) {
                        valueLocationWasWritten = true
                    }
                }

                if (valueLocationWasWritten) {
                    val locSet = Set(assignmentLocation)
                    UnionValue(
                        propertyNames.iterator map { Ref(target, _, locSet) }
                    )
                } else {
                    value
                }
        }

        val resultHeap = heapAfterCoersion.end(writeMutator)

        val resultState = state.copy(heap = resultHeap)

        Some((result, resultState))
    }

    /*protected def write(target: ObjectLike, propertyName: String, value: Entity): Entity = {

    }*/
    private def dynWrite(base: Entity,
                         obj: ObjectLike,
                         valueLocation: ValueLocation,
                         value: Entity,
                         onlyNumbers: Boolean,
                         mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Unit = {
        mutator.writeToProperties(obj, valueLocation, onlyNumbers, value)
    }

    // returns whether a property was changed
    private def write(base: Entity, obj: ObjectLike, propertyName: String, valueLocation: =>ValueLocation, value: Entity, onlyOneTarget: Boolean, mutator: Heap.Mutator)(implicit analysis: DataFlowAnalysis): Boolean = {

        mutator.writeToProperty(obj, propertyName, valueLocation, isCertainWrite = onlyOneTarget, value) match {
            case AbstractProperty(_, _, _, _, _, setter, _) =>
                if (setter != NeverValue && setter != AnyEntity) {
                    ???
                }
                false

            case ConcreteProperty(_, _, _, _, _, setter) =>
                if (setter.nonEmpty) {
                    // todo: implement setter
                    ???
                }
                true
        }
    }
}
