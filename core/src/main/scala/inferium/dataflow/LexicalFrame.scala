package inferium.dataflow

import inferium.lattice.{Entity, ObjectEntity}

case class LexicalFrame(obj: ObjectEntity, outer: Option[LexicalFrame] = None) {
    val depth: Int = outer map { _.depth + 1 } getOrElse 0
    val objects: Vector[ObjectEntity] = (outer map { _.objects } getOrElse Vector()) :+ obj

    def ::(obj: ObjectEntity): LexicalFrame = LexicalFrame(obj, Some(this))
}
