package inferium.lattice

import inferium.utils.Counter

final class Location private (val id: Long) extends AnyVal

object Location {
    def unapply(arg: Location): Option[Long] = Some(arg.id)


    private val counter = new Counter()

    def create(): Location = {
        return new Location(counter.next())
    }

    def apply(): Location = create()
}
