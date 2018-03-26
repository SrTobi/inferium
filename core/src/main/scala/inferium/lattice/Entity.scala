package inferium.lattice

import scala.collection.mutable
import scala.collection.immutable


abstract class Entity {

    def unify(other: Entity): Entity = Entity.unify(this, other)

    def mightBe(entity: Entity): Boolean = this == entity
}

object Entity {
    /*def unify(entities: Seq[Entity]): Entity = {
        var hasUndefined = false
        var hasNull = false
        var boolValue: BoolValue = null
        var numberValue: NumberValue = null
        var stringValues = mutable.Set.empty[StringValue]
        var objLocations = mutable.Set.empty[ObjLocation]
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

    def apply(entities: Entity*): Entity = unify(entities)

    private val emptyUnion = UnionValue(isUndef = false, isNull = false, None, Set.empty, Set.empty, Set.empty)
    private val stringUnion: Set[StringValue] = immutable.Set(StringValue)

    def unify(entities: Seq[Entity]): Entity = entities match {
        case Seq() => NeverValue
        case fst +: rest => unify(fst, rest)
    }

    private def unify(entity: Entity, entities: Seq[Entity]): Entity = entities match {
        case Seq() => entity
        case fst +: rest => unify(unify(entity, fst), rest)
    }

    def unify(fst: Entity, snd: Entity): Entity = (fst, snd) match {
        case (fst: UnionValue, snd: UnionValue) =>
            return UnionValue(
                fst.isUndef || snd.isUndef,
                fst.isNull || snd.isNull,
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
        case UnionValue(undef, isNull, num, strings, objs, refs) => entity match {
            case NeverValue => union
            case UndefinedValue => UnionValue(isUndef = true, isNull, num, strings, objs, refs)
            case NullValue => UnionValue(undef, isNull = true, num, strings, objs, refs)
            case number: NumberValue => UnionValue(undef, isNull, if (num.contains(number)) num else Some(NumberValue), strings, objs, refs)
            case str: SpecificStringValue => UnionValue(undef, isNull, num, if (strings == stringUnion) stringUnion else strings + str, objs, refs)
            case StringValue => UnionValue(undef, isNull, num, stringUnion, objs, refs)
            case obj: ObjLocation => UnionValue(undef, isNull, num, strings, objs + obj, refs)
            case ref: Ref => UnionValue(undef, isNull, num, strings, objs, refs + ref)
            case _ => throw new IllegalArgumentException(s"Unexpected entity $entity")
        }
    }
}