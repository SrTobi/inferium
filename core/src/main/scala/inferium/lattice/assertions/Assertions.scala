package inferium.lattice.assertions

import inferium.lattice.Entity

sealed abstract class Assertion

object Truthyfied extends Assertion
object Falsyfied extends Assertion
final case class HasProperty(name: String) extends Assertion