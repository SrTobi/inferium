package inferium.lattice

import inferium.lattice.ObjectType.OrdinaryObject

case class ObjectEntity(loc: Location, objectType: ObjectType) extends Entity {
    override def isNormalized: Boolean = true
    override def normalized(heap: Heap.Mutator): Entity = this
}

object ObjectEntity {
    def ordinary(loc: Location): ObjectEntity = ObjectEntity(loc, OrdinaryObject)
}


sealed abstract class ObjectType
object ObjectType {
    case object OrdinaryObject extends ObjectType
}