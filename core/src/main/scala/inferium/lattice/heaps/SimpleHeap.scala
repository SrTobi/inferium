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
        import SimpleHeap.mergeProperties
        val allHeaps = (this +: heaps) map { _.asInstanceOf[SimpleHeap] }
        val newObjects = Utils.mergeMaps(allHeaps map { _.objects }: _*)() {
            case (Obj(ap1, cp1, c1), Obj(ap2, cp2, c2)) =>
                Obj(mergeProperties(ap1, ap2), mergeProperties(cp1, cp2), Math.max(c1, c2))
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
    private type Properties = Map[String, Property]
    class Obj(val abstractProperties: Properties, val concreteProperties: Properties, val abstractCount: Long) {
        def isAbstractObject(o: ObjectLike): Boolean = abstractCount != o.abstractCount
        def propertiesFor(o: ObjectLike): Properties = if (isAbstractObject(o)) abstractProperties else concreteProperties

        override def equals(o: scala.Any): Boolean = o match {
            case other: Obj =>
                abstractProperties == other.abstractProperties && concreteProperties == other.concreteProperties

            case _ =>
                false
        }
    }

    object Obj {
        def apply(abstractProperties: Properties, concreteProperties: Properties, abstractCount: Long): Obj = new Obj(abstractProperties, concreteProperties, abstractCount)
        def unapply(arg: Obj): Option[(Properties, Properties, Long)] = Some((arg.abstractProperties, arg.concreteProperties, arg.abstractCount))
    }

    private def mergeProperties(p1: Properties, p2: Properties): Properties = {
        Utils.mergeMaps(p1, p2)(a => a)({ _ unify _ })
    }

    private class SimpleMutator(var objects: Map[Location, Obj] = Map.empty, var boxedValues: Map[ValueLocation, Entity]) extends Mutator {
        override def allocObject(location: Location, creator: (Location, Long) => ObjectLike): ObjectLike = {
            val ac = objects.get(location) match {
                case Some(Obj(abstractProps, concreteProps, oldAbstractCount)) =>
                    val abstractCount = oldAbstractCount + 1
                    objects += location -> Obj(mergeProperties(abstractProps, concreteProps), Map.empty, abstractCount)

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

        override def setProperty(obj: ObjectLike, propertyName: String, property: Property): Unit = {
            assert(property != Property.absentProperty)
            objects.get(obj.loc) match {
                case Some(desc@Obj(abstractProps, concreteProps, abstractCount)) =>
                    // we found the object, now set the property
                    if (desc.isAbstractObject(obj)) {
                        val newProperties = abstractProps + (propertyName -> property)
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

        override def getProperty(obj: ObjectLike, propertyName: String): Property = {
            objects.get(obj.loc) match {
                case Some(objDesc) =>

                    // we found the object, now get the property
                    objDesc.propertiesFor(obj).get(propertyName) match {
                        case Some(prop) =>
                            prop

                        case None =>
                            // TODO: search prototypes
                            Property.absentProperty
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