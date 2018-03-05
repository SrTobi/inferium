package inferium.lattice

sealed abstract class Property(val configurable: BoolLattice, val enumerable: BoolLattice) {
    def unify(other: Property): Property
}

object Property {
    def unify(data: DataProperty, accessor: PureAccessorProperty): Property = {
        new PropertyTop(
            data.configurable.unify(accessor.configurable),
            data.enumerable.unify(accessor.enumerable),
            data.value,
            data.writable,
            accessor.get,
            accessor.set
        )
    }
}

sealed class DataProperty(configurable: BoolLattice,
                          enumerable: BoolLattice,
                          val value: Entity,
                          val writable: BoolLattice) extends Property(configurable, enumerable) {
    override def unify(other: Property): Property = other match {
        case top: PropertyTop =>
            PropertyTop.unify(top, this)
        case other: DataProperty =>
            new DataProperty(
                this.configurable.unify(other.configurable),
                this.enumerable.unify(other.enumerable),
                this.value.unify(other.value),
                this.writable.unify(other.writable)
            )
        case accessor: PureAccessorProperty =>
            Property.unify(this, accessor)
    }
}

sealed trait AccessorProperty extends Property {
    val get: Entity
    val set: Entity
}

sealed class PureAccessorProperty(configurable: BoolLattice,
                                  enumerable: BoolLattice,
                                 // TODO: get and set should be FunctionEntities
                                  override val get: Entity,
                                  override val set: Entity) extends Property(configurable, enumerable) with AccessorProperty {
    override def unify(other: Property): Property = other match {
        case top: PropertyTop =>
            PropertyTop.unify(top, this)
        case data: DataProperty =>
            Property.unify(data, this)
        case other: PureAccessorProperty =>
            new PureAccessorProperty(
                this.configurable.unify(other.configurable),
                this.enumerable.unify(other.enumerable),
                this.get.unify(other.get),
                this.set.unify(other.set)
            )
    }
}

sealed class PropertyTop(configurable: BoolLattice,
                         enumerable: BoolLattice,
                         value: Entity,
                         writable: BoolLattice,
                         override val get: Entity,
                         override val set: Entity) extends DataProperty(configurable, enumerable, value, writable) with AccessorProperty {
    override def unify(other: Property): Property = other match {
        case other: PropertyTop =>
            new PropertyTop(
                this.configurable.unify(other.configurable),
                this.enumerable.unify(other.enumerable),
                this.value.unify(other.value),
                this.writable.unify(other.writable),
                this.get.unify(other.get),
                this.set.unify(other.set)
            )
        case data: DataProperty =>
            PropertyTop.unify(this, data)
        case accessor: PureAccessorProperty =>
            PropertyTop.unify(this, accessor)
    }
}

object PropertyTop {
    def unify(top: PropertyTop, data: DataProperty): PropertyTop = {
        new PropertyTop(
            data.configurable.unify(top.configurable),
            data.enumerable.unify(top.enumerable),
            top.value.unify(data.value),
            top.writable.unify(data.writable),
            top.get,
            top.set
        )
    }

    def unify(top: PropertyTop, accessor: PureAccessorProperty): PropertyTop = {

        new PropertyTop(
            accessor.configurable.unify(top.configurable),
            accessor.enumerable.unify(top.enumerable),
            top.value,
            top.writable,
            top.get,
            accessor.set.unify(top.set)
        )
    }
}