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

        def allocObject(location: Location, creator: (Location, Long) => ObjectLike): ObjectLike
        def allocOrdinaryObject(location: Location): ObjectLike = allocObject(location, (loc, ac) => OrdinaryObjectEntity(loc)(ac))
        def isConcreteObject(obj: ObjectLike): Boolean
        def setProperty(obj: ObjectLike, propertyName: String, property: Property)
        def getProperty(obj: ObjectLike, propertyName: String): Property
        //def listProperties(obj: ObjectLike): Seq[Entity]

        def getValue(loc: ValueLocation): Entity
        def setValue(loc: ValueLocation, value: Entity): Unit

        def getPropertyValueIgnoringGetter(obj: ObjectLike, propertyName: String): Entity = {
            val Property(_, _, value, _, getter, _) = getProperty(obj, propertyName)
            val result = value.toSeq map getValue
            UnionValue(result)
        }

        def forceSetPropertyValue(obj: ObjectLike, propertyName: String, writeLoc: ValueLocation, value: Entity): Unit = {
            setProperty(obj, propertyName, Property.defaultWriteToObject(Set(writeLoc)))
            setValue(writeLoc, value)
        }
    }

    abstract class Factory {
        def create(): Heap
    }
}