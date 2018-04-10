package inferium.lattice

import inferium.lattice.Entity.stringUnion

case class UnionValue(isUndef: Boolean, isNull: Boolean, bool: GeneralBoolLattice, num: Option[NumberValue], strings: Set[StringValue], objs: Set[ObjectEntity], refs: Set[Ref]) extends Entity {
    override def mightBe(entity: Entity): Boolean = ??? /*entity match {
        case NeverValue => true
        case UndefinedValue => isUndef
        case NullValue => isNull
        case b: BoolValue => bool.mightBe(b)
        case number: NumberValue => num.contains(number)
        case str: StringValue => strings.contains(str)
        case obj: ObjectEntity => objs.contains(obj)
        case ref: Ref => refs.contains(ref)
        case _ => throw new IllegalArgumentException(s"Unexpected entity $entity")
    }*/
}
