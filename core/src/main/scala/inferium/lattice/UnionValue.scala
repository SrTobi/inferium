package inferium.lattice

import inferium.lattice.Entity.stringUnion

case class UnionValue(isUndef: Boolean, isNull: Boolean, num: Option[NumberValue], strings: Set[StringValue], objs: Set[ObjLocation], refs: Set[Ref]) extends Entity {
    override def mightBe(entity: Entity): Boolean = entity match {
        case NeverValue => true
        case UndefinedValue => isUndef
        case NullValue => isNull
        case number: NumberValue => num.contains(number)
        case str: StringValue => strings.contains(str)
        case obj: ObjLocation => objs.contains(obj)
        case ref: Ref => refs.contains(ref)
        case _ => throw new IllegalArgumentException(s"Unexpected entity $entity")
    }
}
