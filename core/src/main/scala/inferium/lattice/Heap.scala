package inferium.lattice

import inferium.lattice.Heap.Mutator

abstract class Heap {
    def begin(location: Location): Mutator
    def end(actor: Mutator): Heap

    def split(): Heap
    def unify(heap: Heap): Heap = unify(Seq(heap))
    def unify(heaps: Seq[Heap]): Heap

    def fixpointUnify(futureHeap: Heap): Heap
}


object Heap {
    sealed class PropertyMutationResult
    case class SuccessfulPropertyMutation(result: Ref) extends PropertyMutationResult

    abstract class Mutator {
        def allocObject(location: ObjectEntity): Unit
        def defineProperty(obj: ObjectEntity, property: String, descriptor: Property)
        def setProperty(obj: ObjectEntity, propertyName: String, property: Property)
        def getProperty(obj: ObjectEntity, propertyName: String): AbstractProperty
        //def listProperties(obj: ObjectEntity): Seq[Entity]

        def getValue(loc: ValueLocation): Entity
        def setValue(loc: ValueLocation, value: Entity): Unit
    }

    abstract class Factory {
        def create(): Heap
    }
}