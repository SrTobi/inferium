package inferium.lattice

import scala.collection.mutable

case class UnionValue(entities: Seq[Entity]) extends Entity {
    override def mightBe(entity: Entity): Boolean = entity match {
        case NeverValue => true
        case SpecificBoolValue(_) if entities.contains(BoolValue) => true
        case SpecificNumberValue(_) if entities.contains(NumberValue) => true
        case SpecificStringValue(_) if entities.contains(StringValue) => true
        case union: UnionValue => union.entities forall { entity => this mightBe entity }
        case _ => entities.contains(entity)
        //case _ => throw new IllegalArgumentException(s"Unexpected entity $entity")
    }

    override def isNormalized: Boolean = entities.forall(_.isNormalized)
    override def normalized(heap: Heap.Mutator): Entity = UnionValue(entities map { _.normalized(heap) })

    override def toString: String = entities.mkString("{", " | ", "}")
}

object UnionValue {
    def apply(entity: Entity, entities: Entity*): Entity = apply(entity +: entities)
    def apply(entities: Seq[Entity]): Entity = {
        var hasUndefined = false
        var hasNull = false
        var boolValue: BoolValue = null
        var numberValue: NumberValue = null
        var stringValues = mutable.SortedSet.empty[SpecificStringValue](Ordering.by(_.value))
        var objLocations = mutable.Set.empty[ObjectEntity]
        var refs = mutable.Set.empty[Ref]

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

    private def unionFromSeq(entities: Seq[Entity]): Entity = entities match {
        case Seq() => NeverValue
        case Seq(entity) => entity
        case _ => new UnionValue(entities)
    }
}