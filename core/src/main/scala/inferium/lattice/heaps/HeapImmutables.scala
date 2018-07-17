package inferium.lattice.heaps

import inferium.lattice._
import inferium.lattice.heaps.SimpleHeap.SimpleMutator
import inferium.utils.Utils

trait HeapImmutables {

    protected type PropertyKey = String

    protected abstract class InternalFields {
        def dynamicProp: Property
        def prototype: Seq[ObjectLike]
    }

    // todo: implement a mock heap mutator
    protected def unifyObjectLists(l1: Seq[ObjectLike], l2: Seq[ObjectLike]) = Entity.unify(l1.iterator ++ l2.iterator).coerceToObjects(null)

    protected case class ConcreteInternalFields(dynamicProp: AbstractProperty, prototype: Seq[ObjectLike]) extends InternalFields {
        def unify(other: ConcreteInternalFields): ConcreteInternalFields = {
            ConcreteInternalFields(
                dynamicProp unify other.dynamicProp,
                unifyObjectLists(prototype, other.prototype)
            )
        }

        def abstractify(heap: Heap.Mutator): AbstractInternalFields = AbstractInternalFields(dynamicProp.abstractify(heap), prototype)
    }
    protected case class AbstractInternalFields(dynamicProp: AbstractProperty, prototype: Seq[ObjectLike]) extends InternalFields {
        def unify(other: AbstractInternalFields): AbstractInternalFields = {
            AbstractInternalFields(
                dynamicProp unify other.dynamicProp,
                unifyObjectLists(prototype, other.prototype)
            )
        }
    }

    protected type ConcreteProperties = Map[PropertyKey, ConcreteProperty]
    protected type ConcreteDesc = (ConcreteInternalFields, ConcreteProperties)
    protected type AbstractProperties = Map[PropertyKey, AbstractProperty]
    protected type AbstractDesc = (AbstractInternalFields, AbstractProperties)
    protected type Properties = Map[PropertyKey, Property]
    protected type Desc = (InternalFields, Properties)

    protected class Obj(val abstractDesc: AbstractDesc, val concreteDesc: ConcreteDesc, val abstractCount: Long) {
        def isAbstractObject(o: ObjectLike): Boolean = abstractCount != o.abstractCount
        def descFor(o: ObjectLike): Desc = if (isAbstractObject(o)) abstractDesc else concreteDesc

        override def hashCode(): Int = abstractDesc.hashCode() ^ concreteDesc.hashCode()

        override def equals(o: scala.Any): Boolean = o match {
            case other: Obj =>
                (this eq other) || (abstractDesc == other.abstractDesc && concreteDesc == other.concreteDesc)

            case _ =>
                false
        }
    }

    protected object Obj {
        def apply(abstractDesc: AbstractDesc, concreteDesc: ConcreteDesc, abstractCount: Long): Obj = new Obj(abstractDesc, concreteDesc, abstractCount)
        def unapply(arg: Obj): Option[(AbstractDesc, ConcreteDesc, Long)] = Some((arg.abstractDesc, arg.concreteDesc, arg.abstractCount))
    }

    protected def mergeAbsProperties(p1: AbstractProperties, p2: AbstractProperties): AbstractProperties = {
        if (p1 eq p2)
            p1
        else
            Utils.mergeMapsWithMapper(p1, p2)(a => a.withAbsent)({ _ unify _ })
    }

    protected def mergeConcreteProperties(p1: ConcreteProperties, p2: ConcreteProperties): ConcreteProperties = {
        if (p1 eq p2)
            p1
        else
            Utils.mergeMapsWithMapper(p1, p2)(a => a.withAbsent)({ _ unify _ })
    }

    protected def mergeAbsDesc(d1: AbstractDesc, d2: AbstractDesc): AbstractDesc = {
        if (d1 eq d2)
            d1
        else
            (d1._1 unify d2._1, mergeAbsProperties(d1._2, d2._2))
    }

    protected def mergeConcreteDesc(d1: ConcreteDesc, d2: ConcreteDesc): ConcreteDesc = {
        if (d1 eq d2)
            d1
        else
            (d1._1 unify d2._1, mergeConcreteProperties(d1._2, d2._2))
    }

    protected def abstractifyConcreteDesc(desc: ConcreteDesc, mutator: Heap.Mutator): AbstractDesc = {
        val (concreteFields, concreteProps) = desc
        (concreteFields.abstractify(mutator), concreteProps mapValues { _.abstractify(mutator) })
    }

    protected def wasAbstractified(ac: Long): Boolean = ac != 1

    protected val initialAbstractDesc: AbstractDesc = (AbstractInternalFields(AbstractProperty.internalProperty, Seq.empty), Map.empty)

    protected class BoxedValue(val abstractValue: Entity, val concreteValue: Entity, val abstractCount: Long) {
        def isAbstract(loc: ValueLocation): Boolean = loc.abstractCount < abstractCount
        def valueFor(loc: ValueLocation): Entity = if (isAbstract(loc)) abstractValue else concreteValue
        def unify(other: BoxedValue): BoxedValue = {
            if (this eq other)
                this
            else
                BoxedValue(abstractValue unify other.abstractValue, concreteValue unify other.concreteValue, Math.max(abstractCount, other.abstractCount))
        }
        def unifyNormalize(other: BoxedValue, accessor: Heap.Mutator): BoxedValue = {
            BoxedValue((abstractValue unify other.abstractValue).normalized(accessor), (concreteValue unify other.concreteValue).normalized(accessor), Math.max(abstractCount, other.abstractCount))
        }

        override def hashCode(): Int = abstractValue.hashCode() ^ concreteValue.hashCode()

        override def equals(obj: scala.Any): Boolean = obj match {
            case obj: BoxedValue =>
                (this eq obj) || (abstractValue == obj.abstractValue && concreteValue == obj.concreteValue)
            case _ => false
        }
    }

    protected object BoxedValue {
        def apply(abstractValue: Entity, concreteValue: Entity, abstractCount: Long): BoxedValue = new BoxedValue(abstractValue, concreteValue, abstractCount)
        def unapply(arg: BoxedValue): Option[(Entity, Entity, Long)] = Some((arg.abstractValue, arg.concreteValue, arg.abstractCount))
    }

}
