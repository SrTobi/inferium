package inferium.lattice

import inferium.utils.Counter

final class Location private (val id: Long) extends AnyVal {
    override def toString: String = s"#$id"
}

object Location {
    def unapply(arg: Location): Option[Long] = Some(arg.id)


    private val counter = new Counter()

    def create(id: Long) = new Location(id)
    def create(): Location = create(counter.next())
    def apply(): Location = create()
}
