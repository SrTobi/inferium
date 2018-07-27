package inferium.utils

class IdentityKey[K <: AnyRef](val key: K) {
    override def hashCode(): Int = key.hashCode()
    override def equals(o: scala.Any): Boolean = o match {
        case other: IdentityKey[_] =>
            other.key eq key
        case _ =>
            false
    }
}
