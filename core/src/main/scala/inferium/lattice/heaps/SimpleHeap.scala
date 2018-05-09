package inferium.lattice.heaps

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

    override def unify(heaps: Seq[Heap]): Heap = {
        import SimpleHeap.mergeProperties
        val allHeaps = (this +: heaps) map { _.asInstanceOf[SimpleHeap] }
        val newObjects = Utils.mergeMaps(allHeaps map { _.objects }: _*)() {
            case (Obj(ap1, cp1, c1), Obj(ap2, cp2, c2)) =>
                Obj(mergeProperties(ap1, ap2), mergeProperties(cp1, cp2), Math.max(c1, c2))
        }

        val newBoxedValues = Utils.mergeMaps(allHeaps map { _.boxedValues }: _*)() { _ unify _}

        new SimpleHeap(newObjects, newBoxedValues)
    }

    override def fixpointUnify(futureHeaps: Seq[Heap]): Heap = unify(futureHeaps)

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
        def isAbstractObject(o: ObjectEntity): Boolean = abstractCount != o.abstractCount
        def propertiesFor(o: ObjectEntity): Properties = if (isAbstractObject(o)) abstractProperties else concreteProperties

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
        override def allocObject(location: Location): ObjectEntity = {
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
            ObjectEntity(location, ObjectType.OrdinaryObject)(ac)
        }

        override def setProperty(obj: ObjectEntity, propertyName: String, property: Property): Unit = {
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

        override def getProperty(obj: ObjectEntity, propertyName: String): AbstractProperty = {
            objects.get(obj.loc) match {
                case Some(objDesc) =>

                    // we found the object, now get the property
                    objDesc.propertiesFor(obj).get(propertyName) match {
                        case Some(prop) =>
                            prop

                        case None =>
                            // TODO: search prototypes
                            AbsentProperty
                    }

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def defineProperty(obj: ObjectEntity, property: String, descriptor: Property): Unit = ???

        override def getValue(valueLocation: ValueLocation): Entity = boxedValues(valueLocation)
        override def setValue(loc: ValueLocation, value: Entity): Unit = boxedValues += loc -> value
    }
}