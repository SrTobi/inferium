package inferium.utils

import fastparse.noApi._
import fastparse.parsers.Terminals

object GrammarUtils {
    import fastparse.all
    import fastparse.all._

    type UnitP = all.Parser[Unit]

    val unicodeIdStart = CharPred(_.isLetter)
    val unicodeIdContinue = CharPred(c => c.isLetterOrDigit)

    val whiteSpace: UnitP = P(CharIn(" ", "\t", "\u000B", "\u000C", "\u00A0")).opaque("ws")

    val lineTerminator: UnitP = P(CharIn("\n", "\r", "\u2028", "\u2029")).opaque("line-terminator")
    val lineTerminatorSequence: UnitP = P(StringIn("\n", "\r\n", "\r", "\u2028", "\u2029")).opaque("\\n")

    object CStyle {
        val singleLineComment: UnitP = "//" ~/ (!lineTerminator ~ AnyChar).rep
        private val commentEnd = "*/"
        private val noCommentEnd = P(!commentEnd ~ AnyChar)
        val multiLineComment: Parser[Boolean] =
            NoCut(P("/*" ~/
                (!lineTerminator ~/ noCommentEnd).rep ~/
                lineTerminator.map(_ => true).? ~/
                noCommentEnd.rep ~/
                commentEnd
            ).opaque("multiline-comment")).map(_.getOrElse(false))
        val comment: UnitP = P(multiLineComment.map(_ => ()) | singleLineComment).opaque("comment")
        val inlineComment: UnitP = multiLineComment.filter(!_).map(_ => ())


        val ws: UnitP = P(whiteSpace | lineTerminatorSequence | comment).opaque("ws")
        val wsWithLineTerminator: UnitP = P(lineTerminatorSequence | multiLineComment.filter(a => a).map(_ => ()) | singleLineComment).opaque("line-terminating")
        val noLineTerminator: UnitP = P(whiteSpace | inlineComment).opaque("no-line-terminator")
    }


    object ConfigStyle {
        val singleLineComment: UnitP = "#" ~/ (!lineTerminator ~ AnyChar).rep
        val comment: UnitP = singleLineComment.opaque("comment")
        val ws: UnitP = P(whiteSpace | comment | lineTerminatorSequence)
    }

    val booleanLiteral: Parser[Boolean] = P("true").map(_ => true) | P("false").map(_ => false)


    lazy val hex4Digits: UnitP = hexDigit.rep(exactly = 4)
    lazy val codePoint: UnitP = "{" ~ hexDigit.rep(max = 4) ~ "}"
    val unicodeEscapeSequence: UnitP = P("u" ~ (hex4Digits | codePoint))

    val hexDigit: UnitP = CharIn('0' to '9', 'a' to 'f', 'A' to 'F')
    val hexDigits: UnitP = hexDigit.rep(1)
    val hexIntegerLiteral: UnitP = StringInIgnoreCase("0x") ~ hexDigits
    val octalDigit: UnitP = CharIn('0' to '7')
    val octalDigits: UnitP = octalDigit.rep(1)
    val octalIntegerLiteral: UnitP = StringInIgnoreCase("0o") ~ octalDigits
    val binaryDigit: UnitP = CharIn('0' to '1')
    val binaryDigits: UnitP = binaryDigit.rep(1)
    val binaryIntegerLiteral: UnitP = StringInIgnoreCase("0b") ~ binaryDigits
    val decimalDigit: UnitP = CharIn('0' to '9')
    val nonZeroDigit: UnitP = CharIn('1' to '9')
    val decimalDigits: UnitP = decimalDigit.rep(1)
    val signedInteger: UnitP = CharIn("+-").? ~ decimalDigits
    val exponentIndicator: UnitP = CharIn("eE")
    val exponentPart: UnitP = P(exponentIndicator ~ signedInteger)
    val decimalIntegerLiteral: UnitP = P("0" | nonZeroDigit ~ decimalDigits.?)
    val decimalLiteral: UnitP = P(
        decimalIntegerLiteral ~ ("." ~ decimalDigits.?).? ~ exponentPart.? |
            "." ~ decimalDigits ~ exponentPart.?
    )
    val numericLiteral: UnitP = P(decimalLiteral | binaryIntegerLiteral | octalIntegerLiteral | hexIntegerLiteral)


    val hexEscapeSequence: UnitP = "x" ~ hexDigit ~ hexDigit
    val singleEscapeCharacter: UnitP = CharIn("'", "\"", "\\", "bfnrtv")
    val escapeCharacter: UnitP = singleEscapeCharacter | decimalDigit | CharIn("xu")
    val nonEscapeCharacter: UnitP = !(escapeCharacter | lineTerminator) ~ AnyChar
    val characterEscapeSequence: UnitP = singleEscapeCharacter | nonEscapeCharacter
    val escapeSequence: UnitP = characterEscapeSequence | "0" ~ !decimalDigit | hexEscapeSequence | unicodeEscapeSequence
    val lineContinuation: UnitP = "\\" ~ lineTerminatorSequence
    val doubleStringCharacter: UnitP = "\\" ~ escapeSequence | !(CharIn("\"\\") | lineTerminator) ~ AnyChar | lineContinuation
    val singleStringCharacter: UnitP = "\\" ~ escapeSequence | !(CharIn("\'\\") | lineTerminator) ~ AnyChar | lineContinuation
    val stringLiteral: Parser[String] = "\"" ~/ doubleStringCharacter.rep.! ~ "\"" | "\'" ~/ singleStringCharacter.rep.! ~ "\'"
}
