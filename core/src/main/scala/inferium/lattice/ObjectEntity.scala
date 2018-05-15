package inferium.lattice

import inferium.lattice.ObjectType.OrdinaryObject
import inferium.utils.macros.blockRec

case class ObjectEntity(loc: Location, objectType: ObjectType)(val abstractCount: Long) extends Entity {
    override def isNormalized: Boolean = true

    @blockRec(nonrec = true)
    override def normalized(heap: Heap.Mutator): Entity = this

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.True

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectEntity] = Seq(this)
}



sealed abstract class ObjectType
object ObjectType {
    case object OrdinaryObject extends ObjectType
}