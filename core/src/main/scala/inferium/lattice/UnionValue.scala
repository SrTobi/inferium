package inferium.lattice

class UnionValue(val entities: Seq[Entity]) extends Entity {
    assert(entities.collect{ case UnionValue(_) => true }.isEmpty)
}

object UnionValue {
    def apply(entities: Entity*): Entity = Entity.unify(entities)

    def unapply(arg: UnionValue): Option[Seq[Entity]] = Some(arg.entities)
}