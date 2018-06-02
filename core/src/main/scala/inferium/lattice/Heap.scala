package inferium.lattice

import inferium.Unifiable
import inferium.Unifiable.Fixpoint
import inferium.lattice.Heap.Mutator

abstract class Heap extends Unifiable[Heap] {
    def begin(location: Location): Mutator
    def end(actor: Mutator): Heap

    def split(): Heap

    override def unify(other: Heap)(implicit fixpoint: Fixpoint): Heap = unify(Seq(other))
    override def unify(heaps: Seq[Heap])(implicit fixpoint: Fixpoint): Heap
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