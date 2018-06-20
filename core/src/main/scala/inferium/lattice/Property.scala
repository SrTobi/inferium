package inferium.lattice



/*
sealed abstract class AbstractProperty {
    def unify(other: AbstractProperty): AbstractProperty
    def withAbsent: AbstractProperty
}

object AbsentProperty extends AbstractProperty {
    override def unify(other: AbstractProperty): AbstractProperty = other.withAbsent
    override def withAbsent: AbstractProperty = this
}
*/

sealed abstract class Property {
    def configurable: GeneralBoolLattice
    def enumerable: GeneralBoolLattice
    def writable: GeneralBoolLattice
    def mightBeAbsent: Boolean

    def abstractify(heap: Heap.Mutator): AbstractProperty
}

sealed case class ConcreteProperty(configurable: GeneralBoolLattice,
                                   enumerable: GeneralBoolLattice,
                                   value: Set[ValueLocation],
                                   writable: GeneralBoolLattice,
                                   getter: Set[ValueLocation],
                                   setter: Set[ValueLocation]) extends Property {
    def unify(other: ConcreteProperty): ConcreteProperty =
        ConcreteProperty(
            this.configurable.unify(other.configurable),
            this.enumerable.unify(other.enumerable),
            this.value | other.value,
            this.writable.unify(other.writable),
            this.getter | other.getter,
            this.setter | other.setter,
        )

    override def abstractify(heap: Heap.Mutator): AbstractProperty = {
        def toValue(locs: Set[ValueLocation]): Entity = Entity.unify(locs.toSeq.map(loc => heap.getValue(loc).normalized(heap)))
        AbstractProperty(
            configurable,
            enumerable,
            toValue(value),
            writable,
            toValue(getter),
            toValue(setter),
            mightBeAbsent
        )
    }

    def addValue(value: ValueLocation): ConcreteProperty = copy(value = this.value + value)

    override def mightBeAbsent: Boolean = value.contains(ValueLocation.AbsentLocation)
    //def withAbsent: Property = if (mightBeAbsent) this else copy(value = value + ValueLocation.AbsentLocation)
}


sealed case class AbstractProperty(configurable: GeneralBoolLattice,
                                   enumerable: GeneralBoolLattice,
                                   value: Entity,
                                   writable: GeneralBoolLattice,
                                   getter: Entity,
                                   setter: Entity,
                                   mightBeAbsent: Boolean) extends Property  {
    def unify(other: AbstractProperty): AbstractProperty =
        AbstractProperty(
            this.configurable.unify(other.configurable),
            this.enumerable.unify(other.enumerable),
            this.value | other.value,
            this.writable.unify(other.writable),
            this.getter | other.getter,
            this.setter | other.setter,
            this.mightBeAbsent | other.mightBeAbsent
        )

    def addValue(value: Entity): AbstractProperty = copy(value = this.value | value)

    override def abstractify(heap: Heap.Mutator): AbstractProperty = this
}

object ConcreteProperty {
    def defaultWriteToObject(value: Set[ValueLocation], mightBeAbsent: Boolean = false): ConcreteProperty =
        ConcreteProperty(
            configurable = BoolLattice.True,
            enumerable = BoolLattice.True,
            value = if (mightBeAbsent) value + ValueLocation.AbsentLocation else value,
            writable = BoolLattice.True,
            getter = Set.empty,
            setter = Set.empty
        )

    val absentProperty: ConcreteProperty =
        ConcreteProperty(
            configurable = GeneralBoolLattice.Bottom,
            enumerable = GeneralBoolLattice.Bottom,
            value = Set(ValueLocation.AbsentLocation),
            writable = GeneralBoolLattice.Bottom,
            getter = Set.empty,
            setter = Set.empty
        )

    val internalProperty: ConcreteProperty = absentProperty.copy(
        configurable = BoolLattice.True,
        enumerable = BoolLattice.False
    )
}

object AbstractProperty {
    def defaultWriteToObject(value: Entity, mightBeAbsent: Boolean = false): AbstractProperty =
        AbstractProperty(
            configurable = BoolLattice.True,
            enumerable = BoolLattice.True,
            value = value,
            writable = BoolLattice.True,
            getter = NeverValue,
            setter = NeverValue,
            mightBeAbsent = mightBeAbsent
        )

    val absentProperty: AbstractProperty =
        AbstractProperty(
            configurable = GeneralBoolLattice.Bottom,
            enumerable = GeneralBoolLattice.Bottom,
            value = NeverValue,
            writable = GeneralBoolLattice.Bottom,
            getter = NeverValue,
            setter = NeverValue,
            mightBeAbsent = true
        )

    val internalProperty: AbstractProperty =
        absentProperty.copy(
            configurable = BoolLattice.True,
            enumerable = BoolLattice.False
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