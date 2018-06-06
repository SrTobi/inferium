package inferium.lattice.heaps

import inferium.Unifiable
import inferium.lattice.Heap.{Mutator, PropertyMutationResult}
import inferium.lattice.heaps.SimpleHeap.{Obj, SimpleMutator}
import inferium.lattice._
import inferium.utils.Utils

class SimpleHeap(private val objects: Map[Location, Obj] = Map.empty, private val boxedValues: Map[ValueLocation, Entity] = Map.empty) extends Heap {


    override def begin(location: Location): Heap.Mutator = new SimpleMutator(objects, boxedValues)

    override def end(actor: Heap.Mutator): Heap = {
        val mutator = actor.asInstanceOf[SimpleMutator]
        new SimpleHeap(mutator.objects, mutator.boxedValues)
    }

    override def split(): Heap = this

    override def unify(heaps: Seq[Heap])(implicit fixpoint: Unifiable.Fixpoint): Heap = {
        import SimpleHeap._
        val allHeaps = (this +: heaps) map { _.asInstanceOf[SimpleHeap] }
        val newObjects = Utils.mergeMaps(allHeaps map { _.objects }: _*)() {
            case (Obj(ap1, cp1, c1), Obj(ap2, cp2, c2)) =>
                Obj(mergeAbsProperties(ap1, ap2), mergeConcreteProperties(cp1, cp2), Math.max(c1, c2))
        }

        val newBoxedValues = Utils.mergeMaps(allHeaps map { _.boxedValues }: _*)() { _ unify _}

        new SimpleHeap(newObjects, newBoxedValues)
    }

    override def equals(o: scala.Any): Boolean = o match {
        case other: SimpleHeap =>
            objects == other.objects && boxedValues == other.boxedValues

        case _ =>
            false
    }
}


object SimpleHeap {
    private type ConcreteProperties = Map[String, ConcreteProperty]
    private type AbstractProperties = Map[String, AbstractProperty]
    class Obj(val abstractProperties: AbstractProperties, val concreteProperties: ConcreteProperties, val abstractCount: Long) {
        def isAbstractObject(o: ObjectLike): Boolean = abstractCount != o.abstractCount
        def propertiesFor(o: ObjectLike): Map[String, Property] = if (isAbstractObject(o)) abstractProperties else concreteProperties

        override def equals(o: scala.Any): Boolean = o match {
            case other: Obj =>
                abstractProperties == other.abstractProperties && concreteProperties == other.concreteProperties

            case _ =>
                false
        }
    }

    object Obj {
        def apply(abstractProperties: AbstractProperties, concreteProperties: ConcreteProperties, abstractCount: Long): Obj = new Obj(abstractProperties, concreteProperties, abstractCount)
        def unapply(arg: Obj): Option[(AbstractProperties, ConcreteProperties, Long)] = Some((arg.abstractProperties, arg.concreteProperties, arg.abstractCount))
    }

    private def mergeAbsProperties(p1: AbstractProperties, p2: AbstractProperties): AbstractProperties = {
        Utils.mergeMaps(p1, p2)(a => a)({ _ unify _ })
    }

    private def mergeConcreteProperties(p1: ConcreteProperties, p2: ConcreteProperties): ConcreteProperties = {
        Utils.mergeMaps(p1, p2)(a => a)({ _ unify _ })
    }

    private class SimpleMutator(var objects: Map[Location, Obj] = Map.empty, var boxedValues: Map[ValueLocation, Entity]) extends Mutator {
        override def allocObject(location: Location, creator: (Location, Long) => ObjectLike): ObjectLike = {
            val ac = objects.get(location) match {
                case Some(Obj(abstractProps, concreteProps, oldAbstractCount)) =>
                    val abstractCount = oldAbstractCount + 1
                    objects += location -> Obj(mergeAbsProperties(abstractProps, concreteProps.mapValues(_.abstractify(this))), Map.empty, abstractCount)

                    abstractCount
                case None =>
                    val abstractCount = 1
                    objects += location -> Obj(Map.empty, Map.empty, abstractCount)
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
                case Some(desc@Obj(abstractProps, concreteProps, abstractCount)) =>
                    // we found the object, now set the property
                    if (desc.isAbstractObject(obj)) {
                        val newProperties = abstractProps + (propertyName -> property.abstractify(this))
                        objects += obj.loc -> Obj(newProperties, concreteProps, abstractCount)
                    } else {
                        val newProperties = concreteProps + (propertyName -> property)
                        objects += obj.loc -> Obj(abstractProps, newProperties, abstractCount)
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }


        override def writeToProperty(obj: ObjectLike, propertyName: String, valueLoc: ValueLocation, isCertainWrite: Boolean, value: Entity): Property = {
            objects.get(obj.loc) match {
                case Some(desc@Obj(abstractProps, concreteProps, abstractCount)) =>
                    if (desc.isAbstractObject(obj)) {
                        val newProp = abstractProps.get(propertyName) match {
                            case Some(prop) =>
                                prop.copy(value = prop.value | value)

                            case None =>
                                AbstractProperty.defaultWriteToObject(value, mightBeAbsent = true)
                        }

                        val newProperties = abstractProps + (propertyName -> newProp)
                        objects += obj.loc -> Obj(newProperties, concreteProps, abstractCount)
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
                        objects += obj.loc -> Obj(abstractProps, newProperties, abstractCount)
                        newProp
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def getProperty(obj: ObjectLike, propertyName: String): Property = {
            objects.get(obj.loc) match {
                case Some(objDesc) =>

                    // we found the object, now get the property
                    objDesc.propertiesFor(obj).get(propertyName) match {
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