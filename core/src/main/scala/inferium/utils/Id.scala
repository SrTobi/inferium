package inferium.utils

import java.util.concurrent.atomic.AtomicLong

final class Id[T](val id: Long) extends AnyVal {
    override def toString: String = s"#$id"
}

class IdGenerator[T] {
    private val nextId = new AtomicLong(1)
    def newId(): Id[T] = new Id[T](nextId.getAndIncrement())
}
