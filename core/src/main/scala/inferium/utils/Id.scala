package inferium.utils

import java.util.concurrent.atomic.AtomicLong

final class Id[T](val id: Long) extends AnyVal {
    override def toString: String = s"#$id"
}

class IdGenerator[T] {
    private val counter = new Counter()
    def newId(): Id[T] = new Id[T](counter.next())
}
