package inferium.dataflow

import inferium.lattice.{Entity, ObjectLike}

case class LexicalFrame(obj: Entity, outer: Option[LexicalFrame] = None) {
    val depth: Int = outer map { _.depth + 1 } getOrElse 0
    val objects: Vector[Entity] = (outer map { _.objects } getOrElse Vector()) :+ obj

    def ::(obj: Entity): LexicalFrame = LexicalFrame(obj, Some(this))

    override def toString: String = obj + (outer map {" :: " + _ } getOrElse "")
}
