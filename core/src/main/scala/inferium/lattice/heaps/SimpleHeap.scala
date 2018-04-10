package inferium.lattice.heaps

import inferium.lattice.Heap.{Mutator, PropertyMutationResult}
import inferium.lattice.heaps.SimpleHeap.{Obj, SimpleMutator}
import inferium.lattice._

class SimpleHeap(private val objects: Map[Location, Obj] = Map.empty, private val boxedValues: Map[ValueLocation, Entity] = Map.empty) extends Heap {


    override def begin(location: Location): Heap.Mutator = new SimpleMutator(objects, boxedValues)

    override def end(actor: Heap.Mutator): Heap = {
        val mutator = actor.asInstanceOf[SimpleMutator]
        new SimpleHeap(mutator.objects, mutator.boxedValues)
    }

    override def split(): Heap = this

    override def unify(heaps: Seq[Heap]): Heap = ???

    override def fixpointUnify(futureHeap: Heap): Heap = ???
}


object SimpleHeap {
    type Obj = Map[String, Property]

    private class SimpleMutator(var objects: Map[Location, Obj] = Map.empty, var boxedValues: Map[ValueLocation, Entity]) extends Mutator {
        override def allocObject(obj: ObjectEntity): Unit = {
            objects += obj.loc -> Map.empty
        }

        override def setProperty(obj: ObjectEntity, propertyName: String, property: Property): Unit = {
            objects.get(obj.loc) match {
                case Some(properties) =>
                    // we found the object, now set the property
                    val newProperties = properties + (propertyName -> property)
                    objects += obj.loc -> newProperties

                case None =>
                    // TODO: the object does not exist... can that even happen? maybe create a new object?
                    ???
            }
        }

        override def getProperty(obj: ObjectEntity, propertyName: String): AbstractProperty = {
            objects.get(obj.loc) match {
                case Some(properties) =>
                    // we found the object, now get the property
                    properties.get(propertyName) match {
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