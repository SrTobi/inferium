package inferium.lattice.heaps

import java.util

import inferium.Unifiable
import inferium.lattice.Heap.SpecialObjects.SpecialObject
import inferium.lattice.Heap.{Mutator, Shared, SpecialObjectMap}
import inferium.lattice._
import inferium.utils.Utils

import scala.collection.mutable



object ChainHeap extends Heap.Factory with HeapImmutables {
    override def create(config: Heap.Config): (Heap, SpecialObjectMap) = {
        val shared = new Shared(config, mutable.Map.empty)
        val heap = new HeapElement(shared, new HeapRoot(shared))
        (heap, shared.specialObjects)
    }

    private class Shared(config: Heap.Config, specialObjects: SpecialObjectMap) extends Heap.Shared(config, specialObjects)

    private trait HeapProvider {
        def getObject(loc: Location): Option[Obj]
        def getBox(loc: Location): Option[BoxedValue]
    }

    private sealed trait HeapStage extends HeapProvider {
        def shared: Shared
        def depth: Int
        def parent: HeapStage
    }

    private class HeapRoot(override val shared: Shared) extends HeapStage {
        override def depth: Int = 0
        override def parent: HeapStage = this

        override def getObject(loc: Location): Option[Obj] = None
        override def getBox(loc: Location): Option[BoxedValue] = None
    }

    private class HeapElement(override val shared: Shared,
                              val parent: HeapStage,
                              val objects: Map[Location, Obj] = Map.empty,
                              val boxedValues: Map[Location, BoxedValue] = Map.empty) extends Heap with HeapStage {

        override val depth: Int = parent.depth + 1

        private def createMutator(): HeapStageMutator = {
            new HeapStageMutator(this, objects, boxedValues)
        }
        override def begin(location: Location): HeapStageMutator = createMutator()

        override def end(actor: Heap.Mutator): Heap = {
            val mutator = actor.asInstanceOf[HeapStageMutator]
            assert(mutator.origin eq this)
            if ((mutator.objects eq objects) && (mutator.boxedValues eq boxedValues))
                this
            else
                new HeapElement(shared, parent, mutator.objects, mutator.boxedValues)
        }

        override def unify(heaps: Seq[Heap])(implicit fixpoint: Unifiable.Fixpoint): Heap = {
            val allHeaps = (this +: heaps) map { _.asInstanceOf[HeapElement] }
            /*val mutator = createMutator()
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

            new HeapElement(shared, newObjects, newBoxedValues)*/


            val unifiers = new util.IdentityHashMap[HeapStage, Unifier]
            val queue = mutable.PriorityQueue.empty[Unifier](Ordering.by(_.depth))

            def addToUnifiers(unifier: Unifier): Unit = {
                unifiers.get(unifier.parent) match {
                    case null =>
                        unifiers.put(unifier.parent, unifier)
                        queue += unifier
                    case equalUnifier =>
                        equalUnifier.merge(unifier)
                }
            }

            allHeaps.map(new Unifier(_)).foreach(addToUnifiers)

            while(true) {
                val unifier = queue.dequeue()

                if (queue.isEmpty) {
                    return unifier.toHeapElement(shared)
                }

                unifiers.remove(unifier.parent)
                unifier.up()
                addToUnifiers(unifier)
            }

            throw new IllegalStateException("should not be reached")
        }

        override def equals(other: scala.Any): Boolean = other match {
            case other: HeapElement =>
                (other eq this) ||
                    ((parent eq other.parent) && objects == other.objects && boxedValues == other.boxedValues)

            case _ =>
                false
        }

        override def createGlobalHeap(): GlobalHeap = new ChainGlobalHeap(shared, objects, boxedValues)

        override def getObject(loc: Location): Option[Obj] = objects.get(loc).orElse(parent.getObject(loc))
        override def getBox(loc: Location): Option[BoxedValue] = boxedValues.get(loc).orElse(parent.getBox(loc))
    }

    private class Unifier(iniHeap: HeapElement) {
        private val objects =  mutable.Map.empty[Location, Obj]
        private val boxedValues =  mutable.Map.empty[Location, BoxedValue]

        objects ++= iniHeap.objects
        boxedValues ++= iniHeap.boxedValues

        var depth: Int = iniHeap.depth
        var parent: HeapStage = iniHeap.parent
        var current: HeapElement = iniHeap

        private def getObject(loc: Location) = objects.get(loc).orElse(parent.getObject(loc))
        private def getBox(loc: Location) = boxedValues.get(loc).orElse(parent.getBox(loc))

        def up(): Unit = {
            for (entry@(loc, _) <- objects if !objects.contains(loc)) {
                objects += entry
            }

            for (entry@(loc, _) <- boxedValues if !boxedValues.contains(loc)) {
                boxedValues += entry
            }

            depth = parent.depth
            current = parent.asInstanceOf[HeapElement]
            parent = current
        }

        def merge(other: Unifier): Unit = {
            val mutatorForOther = new HeapStageMutator(parent, other.current.objects, other.current.boxedValues)
            val mutatorForMe = new HeapStageMutator(parent, current.objects, current.boxedValues)

            for (entry@(loc, obj1@Obj(ad1, cd1, c1)) <- other.objects) {
                getObject(loc) match {
                    case Some(obj2) if obj2 eq obj1 =>
                        objects += loc -> obj1
                    case Some(obj2@Obj(ad2, cd2, c2)) =>
                        val (abstractDesc, concreteDesc) = if (c1 < c2) {
                            val abstractified = abstractifyConcreteDesc(cd1, mutatorForOther)
                            val newAd1 = if (wasAbstractified(c1)) mergeAbsDesc(abstractified, ad1) else abstractified
                            mergeAbsDesc(newAd1, ad2) -> cd2
                        } else if (c1 > c2) {
                            val abstractified = abstractifyConcreteDesc(cd2, mutatorForMe)
                            val newAd2 = if (wasAbstractified(c2)) mergeAbsDesc(abstractified, ad2) else abstractified
                            mergeAbsDesc(ad1, newAd2) -> cd1
                        } else {
                            mergeAbsDesc(ad1, ad2) -> mergeConcreteDesc(cd1, cd2)
                        }

                        if (c1 != c2 || (abstractDesc ne ad2) || (concreteDesc ne cd2))
                            objects += loc -> Obj(abstractDesc, concreteDesc, Math.max(c1, c2))
                    case None =>
                        objects += loc -> obj1
                }
            }

            for ((loc, otherBox) <- other.boxedValues) {
                getBox(loc) match {
                    case Some(myBox) =>
                        if (myBox ne otherBox) {
                            boxedValues += loc -> (myBox unify otherBox)
                        }
                    case None =>
                        boxedValues += loc -> otherBox
                }
            }
        }

        def toHeapElement(shared: Shared): HeapElement = new HeapElement(shared, parent, objects.toMap, boxedValues.toMap)
    }


    private class GlobalHeapLink(_shared: Shared,
                                 _heapRoot: HeapRoot,
                                 _objects: Map[Location, Obj] = Map.empty,
                                 _boxedValues: Map[Location, BoxedValue] = Map.empty) extends HeapElement(_shared, _heapRoot, _objects, _boxedValues) {

        val objectReads = mutable.Map.empty[Location, Obj]
        val boxReads = mutable.Map.empty[Location, BoxedValue]

        override def getObject(loc: Location): Option[Obj] = {
            val objOpt = objects.get(loc)
            objOpt foreach { objectReads += loc -> _ }
            objOpt
        }
        override def getBox(loc: Location): Option[ChainHeap.BoxedValue] = {
            val boxOpt = boxedValues.get(loc)
            boxOpt foreach { boxReads += loc -> _ }
            boxOpt
        }
    }

    private class ChainGlobalHeap(val shared: Shared,
                                   private var objects: Map[Location, Obj],
                                   private var boxedValues: Map[Location, BoxedValue]) extends GlobalHeap with HeapStage {

        private val heapRoot = new HeapRoot(shared)
        private val links = mutable.Map.empty[Location, GlobalHeapLink]

        override def hasEffect(location: Location): Boolean = {
            for(link <- links.get(location)) {
                for ((objLoc, obj) <- link.objectReads) {
                    objects.get(objLoc) match {
                        case Some(obj2) =>
                            if (obj != obj2)
                                return true
                        case None =>
                            return true
                    }
                }

                for ((boxLoc, box) <- link.boxReads) {
                    boxedValues.get(boxLoc) match {
                        case Some(box2) =>
                            if (box != box2)
                                return true
                        case None =>
                            return true
                    }
                }
                return false
            }
            return true
        }

        override def toHeap(location: Location): Heap = {
            val link = new GlobalHeapLink(shared, heapRoot, objects, boxedValues)
            links += location -> link
            link
        }

        override def feed(heap: Heap): Boolean = {
            val other = heap.asInstanceOf[HeapElement]
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

        override def accessor: HeapStageMutator = new HeapStageMutator(this, objects, boxedValues)
        override def depth: Int = 0
        override def parent: HeapStage = this
        override def getObject(loc: Location): Option[ChainHeap.Obj] = objects.get(loc)
        override def getBox(loc: Location): Option[ChainHeap.BoxedValue] = boxedValues.get(loc)
    }






    private class HeapStageMutator(val origin: HeapStage,
                                   var objects: Map[Location, Obj],
                                   var boxedValues: Map[Location, BoxedValue]) extends Mutator {
        private def shared = origin.shared
        override def config: Heap.Config = shared.config
        override def specialObject(specialObject: SpecialObject): ObjectLike = shared.specialObjects(specialObject)


        def getObject(loc: Location): Option[Obj] = objects.get(loc).orElse(origin.parent.getObject(loc))
        def getBox(loc: Location): Option[BoxedValue] = boxedValues.get(loc).orElse(origin.parent.getBox(loc))

        override def allocObject(location: Location, creator: (Location, Long) => ObjectLike, prototype: Entity): ObjectLike = {
            val prototypeObjs = prototype.coerceToObjects(this)
            val initialConcreteDesc: ConcreteDesc = (ConcreteInternalFields(AbstractProperty.internalProperty, prototypeObjs), Map.empty)

            val ac = getObject(location) match {
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
            getObject(obj.loc) match {
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

            getObject(obj.loc) match {
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

            getObject(obj.loc) match {
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

            getObject(obj.loc) match {
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

            getObject(obj.loc) match {
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
            getObject(obj.loc) match {
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

            getObject(obj.loc) match {
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
            case _ => getBox(valueLocation.loc).get.valueFor(valueLocation)
        }

        override def setValue(loc: Location, value: Entity): ValueLocation = {
            assert(value != NeverValue)
            assert(loc != ValueLocation.AbsentLocation.loc)

            val newBox = getBox(loc) match {
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

            getBox(loc) match {
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