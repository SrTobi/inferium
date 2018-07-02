package inferium.lattice.heaps

import inferium.Unifiable
import inferium.lattice.Heap.{Mutator, SpecialObjectMap}
import inferium.lattice.heaps.SimpleHeap.{Obj, SimpleMutator}
import inferium.lattice._
import inferium.utils.Utils



object SimpleHeap extends Heap.Factory {
    override def create(config: Heap.Config): (Heap, SpecialObjectMap) = {
        val shared = Heap.Shared(config)
        val heap = new SimpleHeapImpl(shared)
        (heap, shared.specialObjects)
    }

    private class SimpleHeapImpl(_shared: Heap.Shared, private val objects: Map[Location, Obj] = Map.empty, private val boxedValues: Map[ValueLocation, Entity] = Map.empty) extends Heap(_shared) {


        override def begin(location: Location): Heap.Mutator = new SimpleMutator(objects, boxedValues, this)

        override def end(actor: Heap.Mutator): Heap = {
            val mutator = actor.asInstanceOf[SimpleMutator]
            new SimpleHeapImpl(shared, mutator.objects, mutator.boxedValues)
        }

        override def split(): Heap = this

        override def unify(heaps: Seq[Heap])(implicit fixpoint: Unifiable.Fixpoint): Heap = {
            val allHeaps = (this +: heaps) map { _.asInstanceOf[SimpleHeapImpl] }
            val newObjects = Utils.mergeMaps(allHeaps map { _.objects }: _*)() {
                case (Obj(ad1, cd1, c1), Obj(ad2, cd2, c2)) =>
                    Obj(mergeAbsDesc(ad1, ad2), mergeConcreteDesc(cd1, cd2), Math.max(c1, c2))
            }

            val newBoxedValues = Utils.mergeMaps(allHeaps map { _.boxedValues }: _*)() { _ unify _}

            new SimpleHeapImpl(shared, newObjects, newBoxedValues)
        }

        override def equals(o: scala.Any): Boolean = o match {
            case other: SimpleHeapImpl =>
                objects == other.objects && boxedValues == other.boxedValues

            case _ =>
                false
        }
    }

    private type PropertyKey = String

    private abstract class InternalFields {
        def dynamicProp: Property
        def base: Set[ObjectLike]
    }
    private case class ConcreteInternalFields(dynamicProp: AbstractProperty, base: Set[ObjectLike]) extends InternalFields {
        def unify(other: ConcreteInternalFields): ConcreteInternalFields = {
            ConcreteInternalFields(
                dynamicProp unify other.dynamicProp,
                base | other.base
            )
        }

        def abstractify(heap: Heap.Mutator): AbstractInternalFields = AbstractInternalFields(dynamicProp.abstractify(heap), base)
    }
    private case class AbstractInternalFields(dynamicProp: AbstractProperty, base: Set[ObjectLike]) extends InternalFields {
        def unify(other: AbstractInternalFields): AbstractInternalFields = {
            AbstractInternalFields(
                dynamicProp unify other.dynamicProp,
                base | other.base
            )
        }
    }

    private type ConcreteProperties = Map[PropertyKey, ConcreteProperty]
    private type ConcreteDesc = (ConcreteInternalFields, ConcreteProperties)
    private type AbstractProperties = Map[PropertyKey, AbstractProperty]
    private type AbstractDesc = (AbstractInternalFields, AbstractProperties)
    private type Properties = Map[PropertyKey, Property]
    private type Desc = (InternalFields, Properties)

    private class Obj(val abstractDesc: AbstractDesc, val concreteDesc: ConcreteDesc, val abstractCount: Long) {
        def isAbstractObject(o: ObjectLike): Boolean = abstractCount != o.abstractCount
        def descFor(o: ObjectLike): Desc = if (isAbstractObject(o)) abstractDesc else concreteDesc

        override def equals(o: scala.Any): Boolean = o match {
            case other: Obj =>
                abstractDesc == other.abstractDesc && concreteDesc == other.concreteDesc

            case _ =>
                false
        }
    }

    private object Obj {
        def apply(abstractDesc: AbstractDesc, concreteDesc: ConcreteDesc, abstractCount: Long): Obj = new Obj(abstractDesc, concreteDesc, abstractCount)
        def unapply(arg: Obj): Option[(AbstractDesc, ConcreteDesc, Long)] = Some((arg.abstractDesc, arg.concreteDesc, arg.abstractCount))
    }

    private def mergeAbsProperties(p1: AbstractProperties, p2: AbstractProperties): AbstractProperties = {
        Utils.mergeMaps(p1, p2)(a => a)({ _ unify _ })
    }

    private def mergeConcreteProperties(p1: ConcreteProperties, p2: ConcreteProperties): ConcreteProperties = {
        Utils.mergeMaps(p1, p2)(a => a)({ _ unify _ })
    }

    private def mergeAbsDesc(d1: AbstractDesc, d2: AbstractDesc): AbstractDesc = {
        (d1._1 unify d2._1, mergeAbsProperties(d1._2, d2._2))
    }

    private def mergeConcreteDesc(d1: ConcreteDesc, d2: ConcreteDesc): ConcreteDesc = {
        (d1._1 unify d2._1, mergeConcreteProperties(d1._2, d2._2))
    }

    private val initialAbstractDesc: AbstractDesc = (AbstractInternalFields(AbstractProperty.internalProperty, Set.empty), Map.empty)

    private class SimpleMutator(var objects: Map[Location, Obj] = Map.empty, var boxedValues: Map[ValueLocation, Entity], override val origin: SimpleHeapImpl) extends Mutator {
        override def allocObject(location: Location, creator: (Location, Long) => ObjectLike, base: Set[ObjectLike]): ObjectLike = {
            val initialConcreteDesc: ConcreteDesc = (ConcreteInternalFields(AbstractProperty.internalProperty, base), Map.empty)

            val ac = objects.get(location) match {
                case Some(Obj(abstractDesc, (concreteFields, conreteProps), oldAbstractCount)) =>
                    val abstractCount = oldAbstractCount + 1
                    val newAbstractDesc = (concreteFields.abstractify(this), conreteProps mapValues { _.abstractify(this) })
                    objects += location -> Obj(mergeAbsDesc(abstractDesc, newAbstractDesc), initialConcreteDesc, abstractCount)

                    abstractCount
                case None =>
                    val abstractCount = 1
                    objects += location -> Obj(initialAbstractDesc, initialConcreteDesc, abstractCount)
                    abstractCount
            }
            creator(location, ac)
        }

        override def isConcreteObject(obj: ObjectLike): Boolean = {
            objects.get(obj.loc) match {
                case Some(Obj(_, _, ac)) =>
                    ac == obj.abstractCount
                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def setProperty(obj: ObjectLike, propertyName: String, property: ConcreteProperty): Unit = {
            assert(property != ConcreteProperty.absentProperty)
            objects.get(obj.loc) match {
                case Some(desc@Obj(abstractDesc@(abstractFields, abstractProps), concreteDesc@(concreteFields, concreteProps), abstractCount)) =>
                    // we found the object, now set the property
                    if (desc.isAbstractObject(obj)) {
                        val newProperties = abstractProps + (propertyName -> property.abstractify(this))
                        objects += obj.loc -> Obj((abstractFields, newProperties), concreteDesc, abstractCount)
                    } else {
                        val newProperties = concreteProps + (propertyName -> property)
                        objects += obj.loc -> Obj(abstractDesc, (concreteFields, newProperties), abstractCount)
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def writeToProperties(obj: ObjectLike, valueLocs: ValueLocation, numbersOnly: Boolean, resolvedValue: Entity): Unit = {
            import Utils._

            def changeExisting(name: String, p: Property): Boolean =
                (numbersOnly ==> StringLattice.isNumberString(name)) &&
                    (config.dynamicWriteAffectsOnlyConfigurable ==> p.configurable.mightBeTrue) &&
                    (config.dynamicWriteAffectsOnlyEnumerable ==> p.enumerable.mightBeTrue)

            objects.get(obj.loc) match {
                case Some(desc@Obj(abstractDesc@(abstractFields, abstractProps), concreteDesc@(concreteFields, concreteProps), abstractCount)) =>
                    if (desc.isAbstractObject(obj)) {
                        val oldDyn = abstractFields.dynamicProp
                        val newDyn = oldDyn.addValue(resolvedValue)
                        var props = abstractProps

                        // change existingProperties
                        if (config.dynamicWriteAffectsExistingProperties) {
                            props
                                .iterator
                                .foreach {
                                    case (name: String, p) if changeExisting(name, p) =>
                                        props += name -> p.addValue(resolvedValue)
                                    case _ =>
                                }
                        }

                        // todo: only copy when changed
                        objects += obj.loc -> Obj((abstractFields.copy(dynamicProp = newDyn), props), concreteDesc, abstractCount)
                    } else {
                        val oldProp = concreteFields.dynamicProp
                        val newProp = oldProp.addValue(resolvedValue)
                        var props = concreteProps

                        // change existingProperties
                        if (config.dynamicWriteAffectsExistingProperties) {
                            props
                                .iterator
                                .foreach {
                                    case (name: String, p) if changeExisting(name, p) =>
                                        props += name -> p.addValue(valueLocs)
                                    case _ =>
                                }
                        }

                        // todo: only copy when changed
                        objects += obj.loc -> Obj(abstractDesc, (concreteFields.copy(dynamicProp = newProp), props), abstractCount)
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def writeToProperty(obj: ObjectLike, propertyName: String, valueLoc: ValueLocation, isCertainWrite: Boolean, resolvedValue: Entity): Property = {
            objects.get(obj.loc) match {
                case Some(desc@Obj(abstractDesc@(abstractFields, abstractProps), concreteDesc@(concreteFields, concreteProps), abstractCount)) =>
                    if (desc.isAbstractObject(obj)) {
                        val newProp = abstractProps.get(propertyName) match {
                            case Some(prop) =>
                                prop.copy(value = prop.value | resolvedValue)

                            case None =>
                                AbstractProperty.defaultWriteToObject(resolvedValue, mightBeAbsent = true)
                        }

                        val newProperties = abstractProps + (propertyName -> newProp)
                        objects += obj.loc -> Obj((abstractFields, newProperties), concreteDesc, abstractCount)
                        newProp
                    } else {
                        val newProp = concreteProps.get(propertyName) match {
                            case Some(prop) =>
                                if (isCertainWrite) prop.copy(value = Set(valueLoc)) else prop.copy(value = prop.value + valueLoc)

                            case None =>
                                // TODO: search prototypes
                                ConcreteProperty.defaultWriteToObject(Set(valueLoc))
                        }

                        val newProperties = concreteProps + (propertyName -> newProp)
                        objects += obj.loc -> Obj(abstractDesc, (concreteFields, newProperties), abstractCount)
                        newProp
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def getProperties(obj: ObjectLike, numbersOnly: Boolean): TraversableOnce[(Option[String], Property)] = {
            import Utils._
            objects.get(obj.loc) match {
                case Some(desc) =>
                    val (fields, properties) = desc.descFor(obj)
                    properties.iterator.filter {
                        case (key: String, p) =>
                            numbersOnly ==> StringLattice.isNumberString(key)
                    }.map {
                        case (key: String, p) => (Some(key), p)
                    } ++ Seq((None, fields.dynamicProp)).iterator

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def getProperty(obj: ObjectLike, propertyName: String): Property = {
            objects.get(obj.loc) match {
                case Some(objDesc) =>

                    // we found the object, now get the property
                    val (fields, properties) = objDesc.descFor(obj)

                    properties.get(propertyName) match {
                        case Some(prop) =>
                            prop

                        case None =>
                            // TODO: search prototypes
                            ConcreteProperty.absentProperty
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def getValue(valueLocation: ValueLocation): Entity = valueLocation match {
            case ValueLocation.AbsentLocation => UndefinedValue
            case _ => boxedValues(valueLocation)
        }
        override def setValue(loc: ValueLocation, value: Entity): Unit = {
            assert(loc != ValueLocation.AbsentLocation)
            boxedValues += loc -> value
        }
    }
}