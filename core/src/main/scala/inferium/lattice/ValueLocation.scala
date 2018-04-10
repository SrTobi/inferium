package inferium.lattice

final class ValueLocation private(val loc: Long) extends AnyVal

object ValueLocation {
    val Scope = ValueLocation(Location())
    def apply(location: Location): ValueLocation = new ValueLocation(location.id)
}