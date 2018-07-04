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


        private def createMutator(): SimpleMutator = new SimpleMutator(objects, boxedValues, this)
        override def begin(location: Location): SimpleMutator = createMutator()

        override def end(actor: Heap.Mutator): Heap = {
            val mutator = actor.asInstanceOf[SimpleMutator]
            new SimpleHeapImpl(shared, mutator.objects, mutator.boxedValues)
        }

        override def split(): Heap = this

        override def unify(heaps: Seq[Heap])(implicit fixpoint: Unifiable.Fixpoint): Heap = {
            val allHeaps = (this +: heaps) map { _.asInstanceOf[SimpleHeapImpl] }
            val mutator = createMutator()
            val newObjects = Utils.mergeMaps(allHeaps map { _.objects }: _*) {
                case (Obj(ad1, cd1, c1), Obj(ad2, cd2, c2)) =>
                    val (abstractDesc, concreteDesc) = if (c1 < c2) {
                        val abstractified = abstractifyConcreteDesc(cd1, mutator)
                        val newAd1 = if (wasAbstractified(c1)) mergeAbsDesc(abstractified, ad1) else abstractified
                        mergeAbsDesc(newAd1, ad2) -> cd2
                    } else if (c1 > c2) {
                        val abstractified = abstractifyConcreteDesc(cd2, mutator)
                        val newAd2 = if (wasAbstractified(c2)) mergeAbsDesc(abstractified, ad2) else abstractified
                        mergeAbsDesc(ad1, newAd2) -> cd1
                    } else {
                        mergeAbsDesc(ad1, ad2) -> mergeConcreteDesc(cd1, cd2)
                    }

                    Obj(abstractDesc, concreteDesc, Math.max(c1, c2))
            }

            val newBoxedValues = Utils.mergeMaps(allHeaps map { _.boxedValues }: _*) { _ unify _}

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
        def prototype: Seq[ObjectLike]
    }

    // todo: implement a mock heap mutator
    private def unifyObjectLists(l1: Seq[ObjectLike], l2: Seq[ObjectLike]) = Entity.unify(l1.iterator ++ l2.iterator).coerceToObjects(null)

    private case class ConcreteInternalFields(dynamicProp: AbstractProperty, prototype: Seq[ObjectLike]) extends InternalFields {
        def unify(other: ConcreteInternalFields): ConcreteInternalFields = {
            ConcreteInternalFields(
                dynamicProp unify other.dynamicProp,
                unifyObjectLists(prototype, other.prototype)
            )
        }

        def abstractify(heap: Heap.Mutator): AbstractInternalFields = AbstractInternalFields(dynamicProp.abstractify(heap), prototype)
    }
    private case class AbstractInternalFields(dynamicProp: AbstractProperty, prototype: Seq[ObjectLike]) extends InternalFields {
        def unify(other: AbstractInternalFields): AbstractInternalFields = {
            AbstractInternalFields(
                dynamicProp unify other.dynamicProp,
                unifyObjectLists(prototype, other.prototype)
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

        override def hashCode(): Int = abstractDesc.hashCode() ^ concreteDesc.hashCode()

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
        Utils.mergeMapsWithMapper(p1, p2)(a => a.withAbsent)({ _ unify _ })
    }

    private def mergeConcreteProperties(p1: ConcreteProperties, p2: ConcreteProperties): ConcreteProperties = {
        Utils.mergeMapsWithMapper(p1, p2)(a => a.withAbsent)({ _ unify _ })
    }

    private def mergeAbsDesc(d1: AbstractDesc, d2: AbstractDesc): AbstractDesc = {
        (d1._1 unify d2._1, mergeAbsProperties(d1._2, d2._2))
    }

    private def mergeConcreteDesc(d1: ConcreteDesc, d2: ConcreteDesc): ConcreteDesc = {
        (d1._1 unify d2._1, mergeConcreteProperties(d1._2, d2._2))
    }

    private def abstractifyConcreteDesc(desc: ConcreteDesc, mutator: SimpleMutator): AbstractDesc = {
        val (concreteFields, concreteProps) = desc
        (concreteFields.abstractify(mutator), concreteProps mapValues { _.abstractify(mutator) })
    }

    private def wasAbstractified(ac: Long): Boolean = ac != 1

    private val initialAbstractDesc: AbstractDesc = (AbstractInternalFields(AbstractProperty.internalProperty, Seq.empty), Map.empty)

    private class SimpleMutator(var objects: Map[Location, Obj] = Map.empty, var boxedValues: Map[ValueLocation, Entity], override val origin: SimpleHeapImpl) extends Mutator {
        override def allocObject(location: Location, creator: (Location, Long) => ObjectLike, prototype: Entity): ObjectLike = {
            val prototypeObjs = prototype.coerceToObjects(this)
            val initialConcreteDesc: ConcreteDesc = (ConcreteInternalFields(AbstractProperty.internalProperty, prototypeObjs), Map.empty)

            val ac = objects.get(location) match {
                case Some(Obj(abstractDesc, concreteDesc, oldAbstractCount)) =>
                    val abstractCount = oldAbstractCount + 1
                    val newAbstractDesc = abstractifyConcreteDesc(concreteDesc, this)
                    val finalAbstractDesc = if (!wasAbstractified(oldAbstractCount)) newAbstractDesc else mergeAbsDesc(abstractDesc, newAbstractDesc)
                    objects += location -> Obj(finalAbstractDesc, initialConcreteDesc, abstractCount)

                    abstractCount
                case None =>
                    val abstractCount = 1
                    objects += location -> Obj(initialAbstractDesc, initialConcreteDesc, abstractCount)
                    abstractCount
            }
            creator(location, ac)
        }

        override def isConcreteObject(obj: ObjectLike): Boolean = {
            if (obj == AnyEntity) {
                return false
            }
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

            if (obj == AnyEntity) {
                return
            }

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

            if (obj == AnyEntity) {
                return
            }

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
            if (obj == AnyEntity) {
                return AbstractProperty.anyProperty
            }

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
            if (obj == AnyEntity) {
                return Iterator(None -> AbstractProperty.anyProperty)
            }

            import Utils._
            objects.get(obj.loc) match {
                case Some(desc) =>
                    val (fields, properties) = desc.descFor(obj)

                    var result = Iterator((None: Option[String], fields.dynamicProp))

                    if (config.dynamicReadRespectsProperties) {
                        result ++= properties.iterator.filter {
                            case (key: String, p) =>
                                numbersOnly ==> StringLattice.isNumberString(key)
                        } map {
                            case (key: String, p) => (Some(key), p)
                        }
                    }

                    if (config.dynamicReadRespectsPrototypes) {
                        result ++= fields.prototype.iterator flatMap { getProperties(_, numbersOnly) }
                    }
                    result

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def getProperty(obj: ObjectLike, propertyName: String): Property = {
            if (obj == AnyEntity) {
                return AbstractProperty.anyProperty
            }

            objects.get(obj.loc) match {
                case Some(objDesc) =>

                    // we found the object, now get the property
                    val (fields, properties) = objDesc.descFor(obj)

                    def foldPrototype(initial: Property): Property =
                        fields.prototype.foldLeft[Property](initial) {
                            case (acc, prototype) =>
                                unifyProperty(acc, getProperty(prototype, propertyName))
                        }

                    properties.get(propertyName) match {
                        case Some(prop) =>
                            if (prop.mightBeAbsent) {
                                foldPrototype(prop)
                            } else {
                                prop
                            }

                        case None =>
                            if (fields.prototype.isEmpty) {
                                ConcreteProperty.absentProperty
                            } else {
                                foldPrototype(ConcreteProperty.bottomProperty)
                            }
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        private def unifyProperty(p1: Property, p2: Property): Property = (p1, p2) match {
            case (p1: ConcreteProperty, p2: ConcreteProperty) =>
                p1 unify p2
            case _ =>
                p1.abstractify(this) unify p2.abstractify(this)
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