package inferium.dataflow

import inferium.dataflow.LexicalEnv.LookupType
import inferium.dataflow.LexicalEnv.{Behavior, LookupItem, LookupType}

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

    /**
      * Creates a chain of lookup items
      *
      * A lookup item consist of
      *     - a type how to do the lookup
      *     - a property to look for in the targeted object
      *     - the index of the lexical frame to look for the correct object
      *
      * @return A chain of lookup items
      */
    final def buildLookupSeq(varName: String): List[LookupItem] = {
        import inferium.dataflow.LexicalEnv.Behavior._

        def superChain: List[LookupItem] = outer match {
            case Some(outerEnv) => outerEnv.buildLookupSeq(varName)
            case None => LookupItem(LookupType.Global, varName, 0) :: Nil
        }


        // check if we can access the lookup item in this env
        behavior match {
            case Hoisted(hasName) =>
                if (hasName(varName)) {
                    LookupItem(LookupType.Declarative, varName, objIdx) :: Nil
                } else {
                    superChain
                }
            case Declarative(mappings) =>
                mappings.get(varName) match {
                    case Some(mapsTo) =>
                        LookupItem(LookupType.Declarative, mapsTo, objIdx) :: Nil
                    case None =>
                        superChain
                }
            case Computed =>
                LookupItem(LookupType.Computed, varName, objIdx) :: superChain
        }
    }
}

object LexicalEnv {
    final case class LookupItem(lookupType: LookupType, property: String, objIdx: Int)

    sealed abstract class LookupType
    object LookupType {
        case object Global extends LookupType
        case object Computed extends LookupType
        case object Declarative extends LookupType
    }

    sealed abstract class Behavior

    object Behavior {
        final case class Hoisted(hoistedMapping: mutable.Set[String]) extends Behavior
        final case class Declarative(mapping: Map[String, String]) extends Behavior
        case object Computed extends Behavior
    }
}