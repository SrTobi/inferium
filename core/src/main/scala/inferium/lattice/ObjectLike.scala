package inferium.lattice

import inferium.dataflow.{CallableInfo, LexicalFrame, Templates}
import inferium.lattice.ObjectType.AnyObject
import inferium.lattice.assertions._
import inferium.utils.macros.blockRec


sealed abstract class ObjectLike extends Entity {
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
    protected[lattice] override def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Boolean, Assertion.Effect) = {
        val result = assertion match {
            case Truthyfied => this
            case Falsyfied => NeverValue
            case Propertyfied(name, has) =>
                val propBool = heap.getPropertyValueIgnoringGetter(this, name).asBoolLattice(heap)
                if (propBool mightBe has) this else NeverValue
        }

        (result, this ne result, Assertion.noEffect(result))
    }

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = Seq(this)

    override def coerceToConstructionObject(heap: Heap.Mutator, constructionObject: ObjectLike): Seq[ObjectLike] = Seq(this)
}

case class OrdinaryObjectEntity(loc: Location)(override val abstractCount: Long) extends ObjectLike {

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice("[object Object]")

    override def objectType: ObjectType = ObjectType.OrdinaryObject

    override def toString: String = s"Obj($loc:$abstractCount)"

    override def withAbstractCount(ac: Long): ObjectLike = OrdinaryObjectEntity(loc)(ac)

    override def coerceToFunctions(heap: Heap.Mutator, fail: () => Unit): Seq[FunctionEntity] = {
        fail()
        Seq.empty
    }
}

case class FunctionEntity(loc: Location, lexicalFrame: LexicalFrame)(override val abstractCount: Long, val callableInfo: CallableInfo) extends ObjectLike {

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice.Top

    override def objectType: ObjectType = ObjectType.FunctionObject

    override def toString: String = s"Func($loc${callableInfo.name map { ":" + _ } getOrElse ""}:$abstractCount)"

    override def withAbstractCount(ac: Long): ObjectLike = FunctionEntity(loc, lexicalFrame)(ac, callableInfo)

    override def coerceToFunctions(heap: Heap.Mutator, fail: () => Unit): Seq[FunctionEntity] = Seq(this)
}

case object AnyEntity extends ObjectLike {
    override def objectType: ObjectType = AnyObject

    override val loc: Location = Location()

    override def abstractCount: Long = 0

    override def withAbstractCount(ac: Long): ObjectLike = {
        assert(ac == 0)
        this
    }

    override def toString: String = "Any"

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): BoolLattice = BoolLattice.Top

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice.Top

    override def coerceToFunctions(heap: Heap.Mutator, fail: () => Unit): Seq[FunctionEntity] = {
        // todo: return a function that takes any and returns any
        ???
    }
}

sealed abstract class ObjectType
object ObjectType {
    case object AnyObject extends ObjectType
    case object OrdinaryObject extends ObjectType
    case object FunctionObject extends ObjectType
}