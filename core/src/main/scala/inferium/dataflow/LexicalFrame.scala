package inferium.dataflow

import inferium.Unifiable
import inferium.lattice.{Entity, ObjectLike}

case class LexicalFrame(obj: Entity, outer: Option[LexicalFrame] = None) extends Unifiable[LexicalFrame] {
    val depth: Int = outer map { _.depth + 1 } getOrElse 0
    val objects: Vector[Entity] = (outer map { _.objects } getOrElse Vector()) :+ obj

    assert(depth == objects.length - 1)

    def ::(obj: Entity): LexicalFrame = LexicalFrame(obj, Some(this))

    override def unify(other: LexicalFrame)(implicit fixpoint: Unifiable.Fixpoint): LexicalFrame = {
        assert(depth == other.depth)

        if (this eq other)
            this
        else {
            val unified = obj unify other.obj
            outer.map { outer => unified :: (outer unify other.outer.get) } getOrElse LexicalFrame(unified)
        }
    }

    override def toString: String = obj + (outer map {" :: " + _ } getOrElse "")
}
