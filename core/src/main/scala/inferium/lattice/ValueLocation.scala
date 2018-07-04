package inferium.lattice

import scala.collection.mutable

final class ValueLocation(val loc: Location, val abstractCount: Long) {
    override def toString: String = loc match {
        case ValueLocation.absentLocationLoc => "absent"
        case _ => s"<$loc:$abstractCount>"
    }

    override def hashCode(): Int = loc.hashCode()

    override def equals(obj: scala.Any): Boolean = obj match {
        case obj: ValueLocation => loc == obj.loc
        case _ => false
    }

    def withAbstractCount(abstractCount: Long): ValueLocation = new ValueLocation(loc, abstractCount)
}

object ValueLocation {
    val AbsentLocation: ValueLocation = new ValueLocation(Location(), 0)
    private val absentLocationLoc = AbsentLocation.loc

    class SetBuilder {
        private val locs = mutable.Map.empty[Long, (Long, Boolean)]

        def add(loc: TraversableOnce[ValueLocation]): Unit = loc.foreach(add)

        def add(loc: ValueLocation): Unit = {
            val id = loc.loc.id
            val ac = loc.abstractCount
            locs.get(id) match {
                case Some((oldMax, hasAbstract)) =>
                    if (ac != oldMax && (ac < oldMax && !hasAbstract)) {
                        locs += id -> (Math.max(ac, oldMax), hasAbstract || ac != oldMax)
                    }
                case None =>
                    locs += id -> (ac, false)
            }
        }

        def toIterator: Iterator[ValueLocation] = locs.iterator.flatMap {
            case (id, (max, hasAbstract)) =>
                val loc = Location.create(id)
                if (hasAbstract)
                    Iterator(new ValueLocation(loc, max), new ValueLocation(loc, 0))
                else
                    Iterator(new ValueLocation(loc, max))
        }

        def toSet: Set[ValueLocation] = toIterator.toSet
        def toSeq: Seq[ValueLocation] = toIterator.toSeq
    }

    def merge(a: TraversableOnce[ValueLocation], b: TraversableOnce[ValueLocation]): Set[ValueLocation] = {
        val builder = new SetBuilder
        builder.add(a)
        builder.add(b)
        builder.toSet
    }

    def add(a: Set[ValueLocation], e: ValueLocation): Set[ValueLocation] = if (a.contains(e)) merge(a, List(e)) else a + e
}