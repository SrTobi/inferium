package inferium.lattice

final class ValueLocation private(val loc: Long) extends AnyVal {
    override def toString: String = s"#$loc"
}

object ValueLocation {
    val Scope = ValueLocation(Location())
    def apply(location: Location): ValueLocation = new ValueLocation(location.id)
}