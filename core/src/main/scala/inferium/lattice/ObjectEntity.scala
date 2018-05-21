package inferium.lattice

import inferium.lattice.ObjectType.OrdinaryObject
import inferium.lattice.assertions._
import inferium.utils.macros.blockRec

case class ObjectEntity(loc: Location, objectType: ObjectType)(val abstractCount: Long) extends Entity {
    override def isNormalized: Boolean = true

    @blockRec(nonrec = true)
    override def normalized(heap: Heap.Mutator): Entity = this

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.True

    //@blockRec(nonrec = true)
    //override def withAssertion(cond: Entity => Boolean, heap: Heap.Mutator): Entity = if (cond(this)) this else NeverValue

    @blockRec(nonrec = true)
    protected[lattice] override def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Iterator[() => Unit]) = (assertion match {
        case Truthyfied => this
        case Falsyfied => NeverValue
        case Propertyfied(name, has) =>
            val propBool = heap.getPropertyValueIgnoringGetter(this, name).asBoolLattice(heap)
            if (propBool mightBe has) this else NeverValue
    }, Iterator())


    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectEntity] = Seq(this)

    override def toString: String = objectType match {
        case OrdinaryObject =>
            s"Obj($loc)"
    }
}


sealed abstract class ObjectType
object ObjectType {
    case object OrdinaryObject extends ObjectType
}