package inferium.lattice

abstract class Entity {

    def unify(other: Entity): Entity = Entity.unify(Seq(this, other))
}

object Entity {
    def unify(entities: Seq[Entity]): Entity = ???
}