package inferium.lattice

import scala.util.matching.Regex


sealed abstract class StringLattice {
    def containsOnlyNumbers: Boolean
    def containsNoNumbers: Boolean

    def unify(other: StringLattice): StringLattice
}

object StringLattice {
    def apply(strings: String*): StringLattice = if (strings.isEmpty) Bottom else SpecificStrings(strings.toSet)


    case object AnyString extends StringLattice {
        override def containsOnlyNumbers: Boolean = false
        override def containsNoNumbers: Boolean = false

        override def unify(other: StringLattice): StringLattice = AnyString
    }
    val Top: AnyString.type = AnyString
    case object NumberString extends StringLattice {
        override def containsOnlyNumbers: Boolean = true
        override def containsNoNumbers: Boolean = false

        override def unify(other: StringLattice): StringLattice = other match {
            case AnyString => AnyString
            case NumberString => NumberString
            case ss: SpecificStrings => if (ss.containsOnlyNumbers) NumberString else AnyString
        }
    }
    /*case object NonNumberString extends StringLattice {
        override def containsOnlyNumbers: Boolean = false
        override def containsNoNumbers: Boolean = true

        override def unify(other: StringLattice): StringLattice = other match {
            case AnyString => AnyString
            case NumberString => NumberString
            case ss: SpecificStrings => if (ss.containsOnlyNumbers) NumberString else AnyString
        }
    }*/
    final case class SpecificStrings(strings: Set[String]) extends StringLattice {
        override lazy val containsOnlyNumbers: Boolean = strings.forall(isNumberString)
        override lazy val containsNoNumbers: Boolean = !strings.exists(isNumberString)

        override def unify(other: StringLattice): StringLattice = other match {
            case AnyString => AnyString
            case NumberString => if (containsOnlyNumbers) NumberString else AnyString
            case SpecificStrings(otherStrings) => SpecificStrings(otherStrings ++ strings)
        }
    }

    val Bottom = SpecificStrings(Set.empty)

    private val NumberRegex: Regex = raw"[0-9]+".r
    def isNumberString(string: String): Boolean = NumberRegex.pattern.matcher(string).matches

    def unify(strings: TraversableOnce[StringLattice]): StringLattice = {

        strings.foldLeft[StringLattice](Bottom) {
            (a, b) =>
                if (a == Top)
                    return Top
                else
                    a unify b
        }
    }
}