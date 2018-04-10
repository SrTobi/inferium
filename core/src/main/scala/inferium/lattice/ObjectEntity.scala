package inferium.lattice

import inferium.lattice.ObjectType.OrdinaryObject

case class ObjectEntity(loc: Location, objectType: ObjectType) extends Entity {

}

object ObjectEntity {
    def ordinary(loc: Location): ObjectEntity = ObjectEntity(loc, OrdinaryObject)
}


sealed abstract class ObjectType
object ObjectType {
    case object OrdinaryObject extends ObjectType
}