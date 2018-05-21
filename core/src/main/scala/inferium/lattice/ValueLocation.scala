package inferium.lattice

final class ValueLocation private(val loc: Long) extends AnyVal {
    override def toString: String = loc match {
        case ValueLocation.AbsentLocation.loc => "absent"
        case _ => s"#$loc"
    }
}

object ValueLocation {
    val AbsentLocation: ValueLocation = new ValueLocation(-1)

    def apply(location: Location): ValueLocation = new ValueLocation(location.id)
}