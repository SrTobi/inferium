package inferium.lattice.heaps

import inferium.Unifiable
import inferium.lattice.Heap.SpecialObjects.SpecialObject
import inferium.lattice.Heap.{Mutator, Shared, SpecialObjectMap}
import inferium.lattice._
import inferium.utils.Utils



object SimpleHeap extends Heap.Factory with HeapImmutables {
    override def create(config: Heap.Config): (Heap, SpecialObjectMap) = {
        val shared = new Heap.Shared(config)
        val heap = new SimpleHeapImpl(shared)
        (heap, shared.specialObjects)
    }

    private class SimpleHeapImpl(override val shared: Heap.Shared,
                                 val objects: Map[Location, Obj] = Map.empty,
                                 val boxedValues: Map[Location, BoxedValue] = Map.empty) extends Heap {


        private def createMutator(): SimpleMutator = new SimpleMutator(objects, boxedValues, shared)
        override def begin(location: Location): SimpleMutator = createMutator()

        override def end(actor: Heap.Mutator): Heap = {
            val mutator = actor.asInstanceOf[SimpleMutator]
            new SimpleHeapImpl(shared, mutator.objects, mutator.boxedValues)
        }

        override def unify(heaps: Seq[Heap])(implicit fixpoint: Unifiable.Fixpoint): Heap = {
            val allHeaps = (this +: heaps) map { _.asInstanceOf[SimpleHeapImpl] }
            // todo: this mutator is problematic, because it might not reference the correct heap
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

        override def createGlobalHeap(): GlobalHeap = new SimpleGlobalHeap(shared, objects, boxedValues)
    }

    private class SimpleGlobalHeap(private val shared: Shared,
                                   private var objects: Map[Location, Obj],
                                   private var boxedValues: Map[Location, BoxedValue]) extends GlobalHeap {

        override def hasEffect(location: Location): Boolean = true
        override def toHeap(location: Location): Heap = new SimpleHeapImpl(shared, objects, boxedValues)

        override def feed(heap: Heap): Boolean = {
            val other = heap.asInstanceOf[SimpleHeapImpl]
            val accessor = this.accessor
            val newObjects = Utils.mergeMaps(objects, other.objects) {
                case (Obj(ad1, cd1, c1), Obj(ad2, cd2, c2)) =>
                    val (abstractDesc, concreteDesc) = if (c1 < c2) {
                        val abstractified = abstractifyConcreteDesc(cd1, accessor)
                        val newAd1 = if (wasAbstractified(c1)) mergeAbsDesc(abstractified, ad1) else abstractified
                        mergeAbsDesc(newAd1, ad2) -> cd2
                    } else if (c1 > c2) {
                        val abstractified = abstractifyConcreteDesc(cd2, accessor)
                        val newAd2 = if (wasAbstractified(c2)) mergeAbsDesc(abstractified, ad2) else abstractified
                        mergeAbsDesc(ad1, newAd2) -> cd1
                    } else {
                        mergeAbsDesc(ad1, ad2) -> mergeConcreteDesc(cd1, cd2)
                    }

                    Obj(abstractDesc, concreteDesc, Math.max(c1, c2))
            }

            val newBoxedValues = Utils.mergeMaps(boxedValues, other.boxedValues) { _ unify _}

            val changed = newBoxedValues != boxedValues || newObjects != objects

            objects = newObjects
            boxedValues = newBoxedValues

            changed
        }

        override def accessor: SimpleMutator = new SimpleMutator(objects, boxedValues, shared)
    }



    private class SimpleMutator(var objects: Map[Location, Obj] = Map.empty, var boxedValues: Map[Location, BoxedValue], val shared: Shared) extends Mutator {
        override def config: Heap.Config = shared.config
        override def specialObject(specialObject: SpecialObject): ObjectLike = shared.specialObjects(specialObject)

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
                                if (isCertainWrite) prop.copy(value = Set(valueLoc)) else prop.addValue(valueLoc)

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

        override def getOwnProperties(obj: ObjectLike): TraversableOnce[(Option[String], Property)] = {
            if (obj == AnyEntity) {
                return Iterator(None -> AbstractProperty.anyProperty)
            }

            objects.get(obj.loc) match {
                case Some(desc) =>
                    val (fields, properties) = desc.descFor(obj)

                    var result = Iterator((None: Option[String], fields.dynamicProp))

                    result ++= properties.iterator.map {
                        case (key: String, p) => (Some(key), p)
                    }

                    result

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
            case _ => boxedValues(valueLocation.loc).valueFor(valueLocation)
        }

        override def setValue(loc: Location, value: Entity): ValueLocation = {
            assert(value != NeverValue)
            assert(loc != ValueLocation.AbsentLocation.loc)

            val newBox = boxedValues.get(loc) match {
                case Some(BoxedValue(abstractValue, _, oldAbstractCount)) =>
                    val newAbstractCount = oldAbstractCount + 1
                    BoxedValue(abstractValue unify value, value, newAbstractCount)
                case None =>
                    BoxedValue(NeverValue, value, 1)
            }
            boxedValues += loc -> newBox
            new ValueLocation(loc, newBox.abstractCount)
        }

        override def changeValue(valueLoc: ValueLocation, value: Entity): Unit = {
            val loc = valueLoc.loc
            assert(value != NeverValue)
            assert(loc != ValueLocation.AbsentLocation.loc)

            boxedValues.get(loc) match {
                case Some(BoxedValue(abstractValue, concreteValue, abstractCount)) =>
                    if (abstractCount == valueLoc.abstractCount) {
                        boxedValues += loc -> BoxedValue(abstractValue, concreteValue, abstractCount)
                    }
                case None =>
                    throw new AssertionError(s"$valueLoc can not be changed, because it does not exist")
            }
        }
    }
}