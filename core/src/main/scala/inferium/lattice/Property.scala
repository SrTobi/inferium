package inferium.lattice


final class AbsentLattice(val mightBeAbsent: Boolean) extends AnyVal {
    def asBool: Boolean = mightBeAbsent

    def unify(other: AbsentLattice): AbsentLattice = AbsentLattice(mightBeAbsent || other.mightBeAbsent)

    override def toString: String = if (mightBeAbsent) "MightBeAbsent" else "NeverAbsent"
}

object AbsentLattice {
    def apply(mightBeAbsent: Boolean): AbsentLattice = new AbsentLattice(mightBeAbsent)

    val MightBeAbsent: AbsentLattice = AbsentLattice(true)
    val NeverAbsent: AbsentLattice = AbsentLattice(false)

    implicit def toBoolean(lattice: AbsentLattice): Boolean = lattice.asBool
}





sealed abstract class AbstractProperty {
    def unify(other: AbstractProperty): AbstractProperty
    def withAbsent: AbstractProperty
}

object AbsentProperty extends AbstractProperty {
    override def unify(other: AbstractProperty): AbstractProperty = other.withAbsent
    override def withAbsent: AbstractProperty = this
}


sealed case class Property(configurable: BoolLattice,
                           enumerable: BoolLattice,
                           mightBeAbsent: AbsentLattice,
                           value: Set[ValueLocation],
                           writable: GeneralBoolLattice,
                           getter: Set[ValueLocation],
                           setter: Set[ValueLocation]) extends AbstractProperty {
    override def unify(other: AbstractProperty): Property = other match {
        case AbsentProperty => withAbsent
        case other: Property =>
            Property(
                this.configurable.unify(other.configurable),
                this.enumerable.unify(other.enumerable),
                this.mightBeAbsent unify other.mightBeAbsent,
                this.value | other.value,
                this.writable.unify(other.writable),
                this.getter | other.getter,
                this.setter | other.setter,
            )
    }

    override def withAbsent: Property = if (mightBeAbsent) this else copy(mightBeAbsent = AbsentLattice.MightBeAbsent)
}

object Property {
    def defaultWriteToObject(value: Set[ValueLocation], mightBeAbsent: Boolean = false): Property = Property(
        configurable = BoolLattice.True,
        enumerable = BoolLattice.True,
        mightBeAbsent = AbsentLattice(mightBeAbsent),
        value = value,
        writable = BoolLattice.True,
        getter = Set.empty,
        setter = Set.empty
    )
}


/*
object Property {
    def unify(data: DataProperty, accessor: PureAccessorProperty): Property = {
        new PropertyTop(
            data.configurable.unify(accessor.configurable),
            data.enumerable.unify(accessor.enumerable),
            data.value,
            data.writable,
            accessor.get,
            accessor.set,
            data.mightBeAbsent unify accessor.mightBeAbsent
        )
    }
}

sealed class DataProperty(configurable: BoolLattice,
                          enumerable: BoolLattice,
                          val value: Entity,
                          val writable: BoolLattice,
                          mightBeAbsent: AbsentLattice) extends Property(configurable, enumerable, mightBeAbsent) {
    override def unify(other: AbstractProperty): Property = other match {
        case top: PropertyTop =>
            PropertyTop.unify(top, this)
        case other: DataProperty =>
            new DataProperty(
                this.configurable unify other.configurable,
                this.enumerable unify other.enumerable,
                this.value unify other.value,
                this.writable unify other.writable,
                this.mightBeAbsent unify other.mightBeAbsent
            )
        case accessor: PureAccessorProperty =>
            Property.unify(this, accessor)
    }

    override def withAbsent: AbstractProperty = ???

    def copy
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
                         override val set: Entity,
                         mightBeAbsent: AbsentLattice) extends DataProperty(configurable, enumerable, value, writable, mightBeAbsent) with AccessorProperty {
    override def unify(other: Property): Property = other match {
        case other: PropertyTop =>
            new PropertyTop(
                this.configurable.unify(other.configurable),
                this.enumerable.unify(other.enumerable),
                this.value.unify(other.value),
                this.writable.unify(other.writable),
                this.get.unify(other.get),
                this.set.unify(other.set),
                this.
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
}*/