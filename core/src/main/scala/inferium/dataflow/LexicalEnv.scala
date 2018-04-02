package inferium.dataflow

import inferium.dataflow.LexicalEnv.Behavior

import scala.annotation.tailrec
import scala.collection.mutable


case class LexicalEnv(outer: Option[LexicalEnv], pushesObject: Boolean, behavior: Behavior) {
    val depth: Int = outer map { _.depth + 1 } getOrElse 0
    val objIdx: Int = (outer map { _.objIdx } getOrElse -1) + (if (pushesObject) 1 else 0)
    assert(objIdx >= 0)

    @tailrec
    final def fixLexicalStack(lexicalFrame: LexicalFrame): LexicalFrame = {
        val targetDepth = objIdx
        assert(targetDepth <= lexicalFrame.depth)

        if (lexicalFrame.depth == targetDepth) {
            return lexicalFrame
        }

        assert(lexicalFrame.outer.isDefined)
        return fixLexicalStack(lexicalFrame.outer.get)
    }
}

object LexicalEnv {
    sealed abstract class Behavior

    object Behavior {
        final case class Hoisted(hoistedMapping: mutable.Set[String]) extends Behavior
        final case class Declarative(mapping: Map[String, String]) extends Behavior
        case object Computed extends Behavior
    }
}