package inferium.lattice

import inferium.Unifiable
import inferium.Unifiable.Fixpoint
import inferium.dataflow.CallableInfo
import inferium.lattice.assertions.Assertion
import inferium.utils.macros.blockRec

import scala.collection.mutable
import scala.collection.immutable


abstract class Entity extends Unifiable[Entity] {

    override def unify(other: Entity)(implicit fixpoint: Fixpoint = Unifiable.noFixpoint): Entity = Entity.unify(this, other)

    def mightBe(entity: Entity): Boolean = this == entity || entity == NeverValue

    def isNormalized: Boolean

    @blockRec
    def normalized(heap: Heap.Mutator): Entity

    @blockRec
    def asBoolLattice(heap: Heap.Mutator): GeneralBoolLattice

    //@blockRec
    //def withAssertion(cond: Entity => Boolean, heap: Heap.Mutator): Entity

    def instituteAssertion(assertion: Assertion, heap: Heap.Mutator): Unit = {
        val (_, _, effects) = gatherAssertionEffects(assertion, heap)
        effects()
    }

    @blockRec
    protected[lattice] def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Boolean, Assertion.Effect)

    def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike]

    def coerceToFunctions(heap: Heap.Mutator, fail: () => Unit): Seq[FunctionEntity]
}

object Entity {
    /*def unify(entities: Seq[Entity]): Entity = {
        var hasUndefined = false
        var hasNull = false
        var boolValue: BoolValue = null
        var numberValue: NumberValue = null
        var stringValues = mutable.Set.empty[StringValue]
        var objLocations = mutable.Set.empty[ObjectLike]
        var refs = mutable.Set.empty[]

        entities.flatMap(unpackUnion) foreach {
            case bool: BoolValue =>
                if (boolValue == null || boolValue == bool)
                    boolValue = bool
                else
                    boolValue = BoolValue
            case number: NumberValue =>
                if (numberValue == null || numberValue == number)
                    numberValue = number
                else
                    numberValue = NumberValue
            case StringValue =>
                stringValues = null
            case string: SpecificStringValue =>
                if (stringValues ne null)
                    stringValues.add(string)
            case UndefinedValue =>
                hasUndefined = true
            case NullValue =>
                hasNull = true
            case NeverValue =>
            case entity =>
                throw new IllegalArgumentException(s"Unknown entity $entity")
        }

        return unionFromSeq(
                (if (hasUndefined) Seq(UndefinedValue) else Seq()) ++
                (if (hasNull) Seq(NullValue) else Seq()) ++
                Option(boolValue).toSeq ++
                Option(numberValue).toSeq ++
                Option(stringValues).getOrElse(Seq(StringValue))
        )
    }*/

    def unify(entity: Entity, entities: Entity*)(implicit fixpoint: Fixpoint): Entity = unify(entity +: entities)

    //private val emptyUnion = UnionValue(isUndef = false, isNull = false, GeneralBoolLattice.Bottom, None, Set.empty, Set.empty, Set.empty)
    //private val stringUnion: Set[StringValue] = immutable.Set(StringValue)

    def unify(entities: Seq[Entity])(implicit fixpoint: Fixpoint): Entity = UnionValue(entities)

    /*private def unify(entity: Entity, entities: Seq[Entity]): Entity = entities match {
        case Seq() => entity
        case fst +: rest => unify(unify(entity, fst), rest)
    }

    def unify(fst: Entity, snd: Entity): Entity = (fst, snd) match {
        case (fst: UnionValue, snd: UnionValue) =>
            return UnionValue(
                fst.isUndef || snd.isUndef,
                fst.isNull || snd.isNull,
                fst.bool unify snd.bool,
                if (fst.num.isEmpty)
                    snd.num
                else if (snd.num.isEmpty || fst.num == snd.num)
                    fst.num
                else
                    Some(NumberValue)
                ,
                if (fst.strings == stringUnion || snd.strings == stringUnion) stringUnion else fst.strings ++ snd.strings,
                fst.objs ++ snd.objs,
                fst.refs ++ snd.refs
            )
        case (fst: UnionValue, _) =>
            unifyUnionWithSingleEntity(fst, snd)

        case (_, snd: UnionValue) =>
            unify(snd, fst)

        case _ =>
            if (fst == snd || fst == NeverValue) {
                return snd
            } else {
                val u = unifyUnionWithSingleEntity(emptyUnion, fst)
                return unifyUnionWithSingleEntity(u, snd)
            }

    }

    private def unifyUnionWithSingleEntity(union: UnionValue, entity: Entity): UnionValue = union match {
        case UnionValue(undef, isNull, bool, num, strings, objs, refs) => entity match {
            case NeverValue => union
            case UndefinedValue => union.copy(isUndef = true)
            case NullValue => union.copy(isNull = true)
            case BoolValue(lattice) => union.copy(bool = bool unify lattice)
            case number: NumberValue => union.copy(num = if (num.contains(number)) num else Some(NumberValue))
            case str: SpecificStringValue => union.copy(strings = if (strings == stringUnion) stringUnion else strings + str)
            case StringValue => union.copy(strings = stringUnion)
            case obj: ObjectLike => union.copy(objs = objs + obj)
            case ref: Ref => union.copy(refs = refs + ref)
            case _ => throw new IllegalArgumentException(s"Unexpected entity $entity")
        }
    }*/
}