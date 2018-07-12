package inferium

import inferium.Unifiable.Fixpoint
import scala.language.implicitConversions

trait Unifiable[T <: Unifiable[T]] {
    this: T =>
    import Unifiable._

    @inline
    final def |(other: T)(implicit fixpoint: Fixpoint = Unifiable.noFixpoint): T = unify(other)(fixpoint)
    def unify(other: T)(implicit fixpoint: Fixpoint = Unifiable.noFixpoint): T

    def unify(other1: T, other2: T, others: T*)(implicit fixpoint: Fixpoint): T = unify(other1 +: other2 +: others)(fixpoint)
    def unify(others: Seq[T])(implicit fixpoint: Fixpoint): T = others.foldLeft(this) { _.unify(_)(fixpoint) }
}

object Unifiable {
    class Fixpoint(val useFixpoint: Boolean = false) extends AnyVal
    object Fixpoint {
        def apply(useFixpoint: Boolean): Fixpoint = new Fixpoint(useFixpoint)
        implicit def toBoolean(fixpoint: Fixpoint): Boolean = fixpoint.useFixpoint
    }

    implicit val noFixpoint: Fixpoint = new Fixpoint(false)
    val useFixpoint: Fixpoint = new Fixpoint(true)
}