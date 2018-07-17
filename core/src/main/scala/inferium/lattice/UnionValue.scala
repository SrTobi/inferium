package inferium.lattice

import inferium.dataflow.CallableInfo
import inferium.lattice
import inferium.lattice.assertions.Assertion
import inferium.utils.macros.blockRec

import scala.collection.mutable

class UnionValue private (val entities: Seq[Entity]) extends Entity {
    assert(entities.length > 1)
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
        GeneralBoolLattice.unify(entities.iterator map { _.asBoolLattice(heap) })
    }

    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = {
        StringLattice.unify(entities.iterator map { _.asStringLattice(heap) })
    }

    @blockRec(nonrec = true)
    override def asProbes(heap: Heap.Mutator): Seq[ProbeEntity] = {
        entities.flatMap { _.asProbes(heap) }
    }

    @blockRec(nonrec = true)
    protected[lattice] override def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Boolean, Assertion.Effect) = {
        var changedEntityEffects: Assertion.Effect = null
        var hasChanged = false
        var remainingEntities = 0

        val es = entities.map(_.gatherAssertionEffects(assertion, heap)).map {
            case (entity, changed, effects) =>
                if (changed) {
                    hasChanged = true
                    if (changedEntityEffects == null && entity != NeverValue) {
                        changedEntityEffects = effects
                    }
                }
                if (entity != NeverValue) {
                    remainingEntities += 1
                }
                entity
        }

        lazy val result = Entity.unify(es)
        if (!hasChanged) {
            (this, false, Assertion.noEffect(this))
        } else if (remainingEntities == 1 && changedEntityEffects != null) {
            (result, true, changedEntityEffects)
        } else {
            (result, true, Assertion.noEffect(result))
        }
    }

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = entities flatMap { _.coerceToObjects(heap) }

    override def coerceToConstructionObject(heap: Heap.Mutator, constructionObject: ObjectLike): Seq[ObjectLike] = {
        entities flatMap { _.coerceToConstructionObject(heap, constructionObject) }
    }

    override def coerceToCallables(heap: Heap.Mutator, fail: () => Unit): Seq[Callable] = entities flatMap { _.coerceToCallables(heap, fail) }

    override def hashCode(): Int = entities.hashCode()

    override def equals(o: scala.Any): Boolean = o match {
        case o: UnionValue =>
            (this eq o) || (entities.length == o.entities.length && (entities forall {e => o.entities.contains(e)}))

        case _ =>
            false
    }

    override def toString: String = entities.mkString("{", " | ", "}")
}

object UnionValue {
    def apply(entity: Entity, entities: Entity*): Entity = apply(entity +: entities)
    def apply(entities: TraversableOnce[Entity]): Entity = {
        var hasAny = false
        var hasUndefined = false
        var hasNull = false
        var boolValue: BoolValue = null
        var numberValue: NumberValue = null
        var stringValues = mutable.SortedSet.empty[SpecificStringValue](Ordering.by(_.value))
        var objLocations = mutable.Map.empty[Location, (Boolean, ObjectLike)]
        var refs = mutable.Map.empty[Ref, Either[Ref, ValueLocation.SetBuilder]]
        val probes = mutable.Set.empty[ProbeEntity]

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
            case AnyEntity =>
                hasAny = true
            case obj: ObjectLike =>
                val v = objLocations.get(obj.loc) match {
                    case Some((hadAbstract, max)) =>
                        val newMax = if (obj.abstractCount > max.abstractCount) obj else max
                        (hadAbstract || obj.abstractCount != max.abstractCount, newMax)

                    case None =>
                        (false, obj)
                }
                objLocations += obj.loc -> v
            case ref: Ref =>
                refs.get(ref) match {
                    case Some(Left(otherRef)) =>
                        val builder = new ValueLocation.SetBuilder
                        builder.add(otherRef.target)
                        builder.add(ref.target)
                        refs += ref -> Right(builder)
                    case Some(Right(builder)) =>
                        builder.add(ref.target)
                    case None =>
                        refs += ref -> Left(ref)
                }
            case probe: ProbeEntity =>
                probes.add(probe)
            case entity =>
                throw new IllegalArgumentException(s"Unknown entity $entity")
        }

        def objLocationSeq = objLocations.toSeq flatMap {
            case (loc, (hadAbstract, max)) =>

                max +: (if (hadAbstract) Seq(max.withAbstractCount(0)) else Seq())
        }

        def refSeq: Seq[Ref] = refs.iterator.map {
            case (_, Left(ref)) => ref
            case (ref, Right(builder)) => ref.copy(target = builder.toSet)
        }.toSeq

        if (hasAny) {
            // keep object locations to be more precise
            unionFromSeq(
                Seq(AnyEntity) ++
                objLocationSeq ++
                refSeq ++
                probes
            )
        } else {
            unionFromSeq(
                (if (hasUndefined) Seq(UndefinedValue) else Seq()) ++
                    (if (hasNull) Seq(NullValue) else Seq()) ++
                    Option(boolValue).toSeq ++
                    Option(numberValue).toSeq ++
                    Option(stringValues).getOrElse(Seq(StringValue)) ++
                    objLocationSeq ++
                    refSeq ++
                    probes
            )
        }
    }

    private def unapply(unionValue: UnionValue): Option[Seq[Entity]] = Some(unionValue.entities)

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