package inferium.lattice

import inferium.dataflow.CallableInfo
import inferium.lattice.assertions.Assertion
import inferium.utils.macros.blockRec

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

    @blockRec(nonrec = true)
    override def normalized(heap: Heap.Mutator): Entity = UnionValue(entities map { _.normalized(heap) })

    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): GeneralBoolLattice = {
        GeneralBoolLattice.unify(entities.view.map(_.asBoolLattice(heap)))
    }

    @blockRec(nonrec = true)
    protected[lattice] override def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Iterator[() => Unit]) = {
        val (es, its) = entities.map(_.gatherAssertionEffects(assertion, heap)).unzip
            (UnionValue(es), its.iterator.flatten)
    }

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = entities flatMap { _.coerceToObjects(heap) }

    override def coerceToFunctions(heap: Heap.Mutator, fail: () => Unit): Seq[FunctionEntity] = entities flatMap { _.coerceToFunctions(heap, fail) }

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
        var objLocations = mutable.Map.empty[Location, (Boolean, ObjectLike)]
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
            case obj: ObjectLike =>
                val v = objLocations.get(obj.loc) match {
                    case Some((hadAbstract, max)) =>
                        lazy val newMax = if (obj.abstractCount > max.abstractCount) obj else max
                        (hadAbstract || obj.abstractCount != max.abstractCount, newMax)

                    case None =>
                        (false, obj)
                }
                objLocations += obj.loc -> v
            case ref: Ref =>
                refs += ref
            case entity =>
                throw new IllegalArgumentException(s"Unknown entity $entity")
        }

        def objLocationSeq = objLocations.toSeq flatMap {
            case (loc, (hadAbstract, max)) =>

                max +: (if (hadAbstract) Seq(max.withAbstractCount(0)) else Seq())
        }

        return unionFromSeq(
            (if (hasUndefined) Seq(UndefinedValue) else Seq()) ++
            (if (hasNull) Seq(NullValue) else Seq()) ++
            Option(boolValue).toSeq ++
            Option(numberValue).toSeq ++
            Option(stringValues).getOrElse(Seq(StringValue)) ++
            objLocationSeq ++
            refs.toSeq
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