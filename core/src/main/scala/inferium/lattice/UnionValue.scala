package inferium.lattice

import inferium.lattice.Entity.stringUnion

case class UnionValue(isUndef: Boolean, isNull: Boolean, bool: GeneralBoolLattice, num: Option[NumberValue], strings: Set[StringValue], objs: Set[ObjectEntity], refs: Set[Ref]) extends Entity {
    override def mightBe(entity: Entity): Boolean = entity match {
        case NeverValue => true
        case UndefinedValue => isUndef
        case NullValue => isNull
        case b: BoolValue => bool == BoolLattice.Top || b.toLattice == bool
        case number: NumberValue => num.contains(number) || num.contains(NumberValue)
        case str: StringValue => strings.contains(str) || strings.contains(StringValue)
        case obj: ObjectEntity => objs.contains(obj)
        case ref: Ref => refs.contains(ref)
        case _: UnionValue => ???
        case _ => throw new IllegalArgumentException(s"Unexpected entity $entity")
    }
}
