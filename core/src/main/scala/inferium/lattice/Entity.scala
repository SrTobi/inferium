package inferium.lattice

import scala.collection.mutable


abstract class Entity {

    def unify(other: Entity): Entity = Entity.unify(Seq(this, other))
}

object Entity {
    def unify(entities: Seq[Entity]): Entity = {
        var boolValue: BoolValue = null
        var numberValue: NumberValue = null
        var stringValues = mutable.Set.empty[StringValue]
        var hasUndefined = false
        var hasNull = false

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
    }

    private def unpackUnion(value: Entity): Seq[Entity] = value match {
        case UnionValue(values) => values
        case NeverValue => Seq()
        case noUnion => Seq(noUnion)
    }

    private def unionFromSeq(entites: Seq[Entity]): Entity = entites match {
        case Seq() => NeverValue
        case Seq(entity) => entity
        case _ => new UnionValue(entites)
    }
}