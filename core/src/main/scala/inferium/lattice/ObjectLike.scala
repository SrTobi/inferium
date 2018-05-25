package inferium.lattice

import inferium.dataflow.{CallableInfo, LexicalFrame, Templates}
import inferium.lattice.assertions._
import inferium.utils.macros.blockRec


abstract class ObjectLike extends Entity {
    def objectType: ObjectType
    def loc: Location
    def abstractCount: Long

    def withAbstractCount(ac: Long): ObjectLike

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


    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = Seq(this)
}

case class OrdinaryObjectEntity(loc: Location)(override val abstractCount: Long) extends ObjectLike {

    override def objectType: ObjectType = ObjectType.OrdinaryObject

    override def toString: String = s"Obj($loc)"

    override def withAbstractCount(ac: Long): ObjectLike = OrdinaryObjectEntity(loc)(abstractCount)

    override def coerceToFunctions(heap: Heap.Mutator, fail: () => Unit): Seq[FunctionEntity] = {
        fail()
        Seq()
    }
}

case class FunctionEntity(loc: Location, lexicalFrame: LexicalFrame)(override val abstractCount: Long, val callableInfo: CallableInfo) extends ObjectLike {
    override def objectType: ObjectType = ObjectType.FunctionObject

    override def toString: String = s"Func($loc${callableInfo.name map { ":" + _ } getOrElse ""})"

    override def withAbstractCount(ac: Long): ObjectLike = FunctionEntity(loc, lexicalFrame)(ac, callableInfo)

    override def coerceToFunctions(heap: Heap.Mutator, fail: () => Unit): Seq[FunctionEntity] = Seq(this)
}


sealed abstract class ObjectType
object ObjectType {
    case object OrdinaryObject extends ObjectType
    case object FunctionObject extends ObjectType
}