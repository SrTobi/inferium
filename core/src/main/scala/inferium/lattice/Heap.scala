package inferium.lattice

import inferium.lattice.Heap.Mutator

abstract class Heap {
    def begin(location: Location): Mutator
    def end(actor: Mutator): Heap

    def split(): Heap
    def unify(heap: Heap): Heap = unify(Seq(heap))
    def unify(heaps: Seq[Heap]): Heap

    def fixpointUnify(futureHeap: Seq[Heap]): Heap
}


object Heap {
    sealed class PropertyMutationResult
    case class SuccessfulPropertyMutation(result: Ref) extends PropertyMutationResult

    abstract class Mutator {
        def allocObject(location: Location): ObjectEntity
        def defineProperty(obj: ObjectEntity, property: String, descriptor: Property)
        def setProperty(obj: ObjectEntity, propertyName: String, property: Property)
        def getProperty(obj: ObjectEntity, propertyName: String): AbstractProperty
        //def listProperties(obj: ObjectEntity): Seq[Entity]

        def getValue(loc: ValueLocation): Entity
        def setValue(loc: ValueLocation, value: Entity): Unit

        def getPropertyValueIgnoringGetter(obj: ObjectEntity, propertyName: String): Entity = getProperty(obj, propertyName) match {
            case AbsentProperty => UndefinedValue
            case Property(_, _, mightBeAbsent, value, _, getter, _) =>
                val result = value.toSeq map getValue
                if (mightBeAbsent) UnionValue(UndefinedValue, result: _*) else UnionValue(result)
        }
    }

    abstract class Factory {
        def create(): Heap
    }
}