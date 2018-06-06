package inferium.lattice.assertions

import inferium.lattice.Entity

sealed abstract class Assertion

object Assertion {
    type Effect = () => Entity

    @inline
    def noEffect(result: Entity): Effect = NoAssertionEffect(result)

    case class NoAssertionEffect(result: Entity) extends Effect {
        override def apply(): Entity = result
        override def toString(): String = s"No Effect[$result]"
    }
}

object Truthyfied extends Assertion
object Falsyfied extends Assertion
final case class Propertyfied(name: String, has: Boolean) extends Assertion