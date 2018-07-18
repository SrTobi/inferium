package inferium.lattice.heaps

import inferium.lattice._
import inferium.lattice.heaps.ChainHeap._
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


    abstract class HeapMutatorImplementation extends Heap.Mutator {

        def getObject(loc: Location): Option[Obj]
        def setObject(loc: Location, obj: Obj): Unit

        def getBox(loc: Location): Option[BoxedValue]
        def setBox(loc: Location, box: BoxedValue): Unit

        override def allocObject[O <: ObjectLike](location: Location, creator: (Location, Long) => O, prototype: Entity): O = {
            val prototypeObjs = prototype.coerceToObjects(this)
            val initialConcreteDesc: ConcreteDesc = (ConcreteInternalFields(AbstractProperty.internalProperty, prototypeObjs), Map.empty)

            val ac = getObject(location) match {
                case Some(Obj(abstractDesc, concreteDesc, oldAbstractCount)) =>
                    val abstractCount = oldAbstractCount + 1
                    val newAbstractDesc = abstractifyConcreteDesc(concreteDesc, this)
                    val finalAbstractDesc = if (!wasAbstractified(oldAbstractCount)) newAbstractDesc else mergeAbsDesc(abstractDesc, newAbstractDesc)
                    setObject(location, Obj(finalAbstractDesc, initialConcreteDesc, abstractCount))

                    abstractCount
                case None =>
                    val abstractCount = 1
                    setObject(location, Obj(initialAbstractDesc, initialConcreteDesc, abstractCount))
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
                        throw new UnsupportedOperationException("can't set concrete property to abstract obj")
                        //val newProperties = abstractProps + (propertyName -> property.abstractify(this))
                        //setObject(obj.loc, Obj((abstractFields, newProperties), concreteDesc, abstractCount))
                    } else {
                        val newProperties = concreteProps + (propertyName -> property)
                        setObject(obj.loc, Obj(abstractDesc, (concreteFields, newProperties), abstractCount))
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def setProperty(obj: ObjectLike, propertyName: String, property: AbstractProperty): Unit = {
            assert(property != AbstractProperty.absentProperty)

            if (obj == AnyEntity) {
                return
            }

            getObject(obj.loc) match {
                case Some(desc@Obj(abstractDesc@(abstractFields, abstractProps), concreteDesc@(concreteFields, concreteProps), abstractCount)) =>
                    // we found the object, now set the property
                    if (desc.isAbstractObject(obj)) {
                        val newProperties = abstractProps + (propertyName -> property)
                        setObject(obj.loc, Obj((abstractFields, newProperties), concreteDesc, abstractCount))
                    } else {
                        throw new UnsupportedOperationException("can't set concrete property to abstract obj")
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
                        setObject(obj.loc, Obj((abstractFields.copy(dynamicProp = newDyn), props), concreteDesc, abstractCount))
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
                        setObject(obj.loc, Obj(abstractDesc, (concreteFields.copy(dynamicProp = newProp), props), abstractCount))
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
                        setObject(obj.loc, Obj((abstractFields, newProperties), concreteDesc, abstractCount))
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
                        setObject(obj.loc, Obj(abstractDesc, (concreteFields, newProperties), abstractCount))
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
            setBox(loc, newBox)
            new ValueLocation(loc, newBox.abstractCount)
        }

        override def changeValue(valueLoc: ValueLocation, value: Entity): Unit = {
            val loc = valueLoc.loc
            assert(value != NeverValue)
            assert(loc != ValueLocation.AbsentLocation.loc)

            getBox(loc) match {
                case Some(BoxedValue(abstractValue, concreteValue, abstractCount)) =>
                    if (abstractCount == valueLoc.abstractCount) {
                        setBox(loc, BoxedValue(abstractValue, concreteValue, abstractCount))
                    }
                case None =>
                    throw new AssertionError(s"$valueLoc can not be changed, because it does not exist")
            }
        }
    }
}
