package com.github.srtobi.inferium.cli.mini

import com.github.srtobi.inferium.cli.mini.Ast.Expression
import com.github.srtobi.inferium.core.UId
import fastparse.core.Parsed.Success
import fastparse.parsers.{Combinators, Terminals}

import scala.collection.mutable
import fastparse.{all, core, noApi}
import fastparse.noApi._

object Ast {

  sealed abstract class Node {
    val id: UId = UId("ast-node")
  }
  sealed abstract class Expression extends Node
  case class UndefinedLiteral() extends Expression
  case class StringLiteral(value: String) extends Expression
  case class BooleanLit(value: Boolean) extends Expression
  case class NumberLit(value: String) extends Expression
  case class Object(properties: Seq[Property]) extends Expression
  case class Identifier(name: String) extends Expression
  case class Property(name: String, init: Expression) extends Node
  case class Operator(op: String, left: Expression, right: Expression) extends Expression
  case class Function(args: Seq[String], block: Block) extends Expression
  case class PropertyAccess(base: Expression, property: String) extends Expression
  case class Call(function: Expression, args: Seq[Expression]) extends Expression

  sealed abstract class Statement extends Node
  type Block = Seq[Statement]
  case class ExpressionStmt(expr: Expression) extends Statement
  case class IfStmt(condition: Expression, success: Block, fail: Option[Block]) extends Statement
  case class ReturnStmt(expression: Option[Expression]) extends Statement
  case class AssignmentStmt(target: Expression, expression: Expression) extends Statement
  case class VarStmt(name: String, init: Expression) extends Statement

  case class Script(main: Block) extends Node
}


object LangTokens {
  import fastparse.all
  import fastparse.all._

  type UnitP = all.Parser[Unit]
  // from # 11.6
  // implementation inspired by http://www.scala-sbt.org/0.12.4/sxr/Parsers.scala.html#326954
  private val unicodeIdStart = CharPred(_.isLetter)
  private val unicodeIdContinue = CharPred(c => c.isLetterOrDigit) /* TODO: Check if this is the correct implementation */

  // # 11.1
  private val zwnj = "\u200C"
  private val zwj = "\u200D"
  private val zwnbsp = "\uFEFF"

  // # 10.1
  val sourceCharacter: Terminals.AnyElem[Char, String] = AnyChar

  // # 11.2
  val whiteSpace: UnitP = P(CharIn(" ", "\t", "\u000B", "\u000C", "\u00A0", zwnbsp /* | Other category "Zs" ???*/)).opaque("ws")

  // # 11.3
  val lineTerminator: UnitP = P(CharIn("\n", "\r", "\u2028", "\u2029")).opaque("line-terminator")
  val lineTerminatorSequence: UnitP = P(StringIn("\n", "\r\n", "\r", "\u2028", "\u2029")).opaque("\\n")

  // # 11.4
  val singleLineComment: UnitP = "//" ~/ (!lineTerminator ~ sourceCharacter).rep
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

  // # from 11.8.4
  lazy val hex4Digits: UnitP = hexDigit.rep(exactly = 4)
  lazy val codePoint: UnitP = "{" ~ hexDigit.rep(max = 4) ~ "}"
  val unicodeEscapeSequence: UnitP = P("u" ~ (hex4Digits | codePoint))

  // # 11.6.2
  val futureReservedWord: Seq[String] = Seq("enum")
  val keyword: Seq[String] = Seq("if", "var", "else", "undefined", "return")
  val reservedWord: Seq[String] = keyword ++ Seq("null", "true", "false") ++ futureReservedWord

  // # 11.6
  val identifierStart: UnitP = P(unicodeIdStart | "$" | "_" | "\\" ~ unicodeEscapeSequence)
  val identifierPart: UnitP = P(unicodeIdContinue | "$" | "_" | "\\" ~ unicodeEscapeSequence | zwnj | zwj)
  val identifierName: Parser[String] = P(unicodeIdStart ~ unicodeIdStart.rep).!.filter(!reservedWord.contains(_))

  // # 11.8.1 / 11.8.2
  val nullLiteral: UnitP = P("null")
  val booleanLiteral: Parser[Boolean] = P("true").map(_ => true) | P("false").map(_ => false)

  // # 11.8.3
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

  // 11.8.4
  val hexEscapeSequence: UnitP = "x" ~ hexDigit ~ hexDigit
  val singleEscapeCharacter: UnitP = CharIn("'", "\"", "\\", "bfnrtv")
  val escapeCharacter: UnitP = singleEscapeCharacter | decimalDigit | CharIn("xu")
  val nonEscapeCharacter: UnitP = !(escapeCharacter | lineTerminator) ~ sourceCharacter
  val characterEscapeSequence: UnitP = singleEscapeCharacter | nonEscapeCharacter
  val escapeSequence: UnitP = characterEscapeSequence | "0" ~ !decimalDigit | hexEscapeSequence | unicodeEscapeSequence
  val lineContinuation: UnitP = "\\" ~ lineTerminatorSequence
  val doubleStringCharacter: UnitP = "\\" ~ escapeSequence | !(CharIn("\"\\") | lineTerminator) ~ sourceCharacter | lineContinuation
  val singleStringCharacter: UnitP = "\\" ~ escapeSequence | !(CharIn("\'\\") | lineTerminator) ~ sourceCharacter | lineContinuation
  val stringLiteral: Parser[String] = "\"" ~/ doubleStringCharacter.rep.! ~ "\"" | "\'" ~/ singleStringCharacter.rep.! ~ "\'"

  //val commonToken: UnitP = identifierName
  val ws: UnitP = P(whiteSpace | lineTerminatorSequence | comment).opaque("ws")
  val wsWithLineTerminator: UnitP = P(lineTerminatorSequence | multiLineComment.filter(a => a).map(_ => ()) | singleLineComment).opaque("line-terminating")
  val noLineTerminator: UnitP = P(whiteSpace | inlineComment).opaque("no-line-terminator")
}


private class LangWsWrapper(WL: P0) {
  implicit def parserApi2[T, V](p0: T)(implicit c: T => P[V]): LangWhitespaceApi[V] =
    new LangWhitespaceApi[V](p0, WL)
}

private object LangWsApi extends LangWsWrapper({
  import fastparse.all._
  import LangTokens._
  ws.rep
})

private class LangWhitespaceApi[+T](p0: P[T], WL: P0) extends fastparse.WhitespaceApi[T](p0, WL) {

  import fastparse.all._
  import fastparse.parsers.Combinators.Sequence
  import fastparse.core.Implicits.Sequencer

  def ~~/[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] =
    Sequence.flatten(Sequence(p0, p, cut = true).asInstanceOf[Sequence[R, R, R, Char, String]])
}


object LangParser {

  import LangTokens._
  import LangWsApi._

  lazy val property: Parser[Ast.Property] = P(
    identifierName ~/ ":" ~/ expression
  ).map((Ast.Property.apply _).tupled)

  lazy val primaryExpression: Parser[Ast.Expression] = P(
    P("undefined").map(_ => Ast.UndefinedLiteral()) |
    booleanLiteral.map(Ast.BooleanLit) |
    numericLiteral.!.map(Ast.NumberLit) |
    identifierName.map(Ast.Identifier) |
    stringLiteral.map(Ast.StringLiteral) |
    "(" ~/ expression ~ ")" |
    ("$" ~/ "(" ~/ identifierName.rep(sep=",") ~ ")" ~ block).map((Ast.Function.apply _).tupled) |
    ("@" ~/ "{" ~/ property.rep(sep=",") ~ "}").map(Ast.Object)
  )

  private def makeInner(left: Expression): Parser[Ast.Expression] = P(
    P("(" ~/ expression.rep(sep=",") ~ ")").map(args => Ast.Call(left, args)) |
      P("." ~/ identifierName).map(Ast.PropertyAccess(left, _))
  ).?.flatMap(_.map(makeInner).getOrElse(Pass.map(_ => left)))

  lazy val innerExpression: Parser[Ast.Expression] = P(
    primaryExpression.flatMap(makeInner)
  )

  lazy val expression: Parser[Ast.Expression] = P(
    (innerExpression ~ (CharIn("+-").! ~ innerExpression).rep).map {
      case (first, tail) => tail.foldLeft(first){
        case (left, (op, right)) => Ast.Operator(op, left, right)
      }
    }
  )

  lazy val statements: Parser[Seq[Ast.Statement]] = statement.rep(sep=";")
  lazy val block: Parser[Seq[Ast.Statement]] = P("{" ~ statements ~ "}")
  lazy val statement: Parser[Ast.Statement] = P(
      ("if" ~/ "(" ~/ expression ~ ")" ~/ block ~/ ("else" ~/ block).?).map((Ast.IfStmt.apply _).tupled) |
      ("return" ~/ expression.?).map(Ast.ReturnStmt) |
      ("var" ~/ identifierName ~/ "=" ~/ expression).map((Ast.VarStmt.apply _).tupled) |
      (NoCut(expression) ~ "=" ~/ expression).map((Ast.AssignmentStmt.apply _).tupled) |
        expression.map(Ast.ExpressionStmt)
  )

  lazy val script: Parser[Ast.Script] = P(Pass ~ statements ~ End).map(Ast.Script)
}

object LangPrinter {
  import Ast._

  def print(node: Node): String = printNode(node, "")

  private def printNode(node: Node, indent: String): String = {
    def p(node: Node) = printNode(node, indent)
    node match {
      case Script(main) => printBlk(main, indent, root = true)
      case IfStmt(cond, success, Some(fail)) => indent + s"if (${p(cond)}) ${printBlk(success, indent)} else ${printBlk(fail, indent)}"
      case IfStmt(cond, success, None) => indent + s"if (${p(cond)}) ${printBlk(success, indent)}"
      case ReturnStmt(expr) => indent + s"return ${expr.map(p).getOrElse("")}"
      case VarStmt(name, init) => indent + s"var $name = ${p(init)}"
      case AssignmentStmt(target, init) => indent + s"${p(target)} = ${p(init)}"
      case Object(properties) => properties.map(p).mkString("@{", ", ", "}")
      case Property(name, init) => s"$name: ${p(init)}"
      case Identifier(id) => id
      case ExpressionStmt(e) => indent + p(e);
      case StringLiteral(str) => "\"" + str + "\""
      case BooleanLit(bool) => bool.toString
      case NumberLit(num) => num
      case Function(args, body) => args.mkString("$(", ",", ")") + printBlk(body, indent)
      case Call(base, args) => p(base) + args.map(p).mkString("(", ",", ")")
      case Operator(op, left, right) => s"(${p(left)} $op ${p(right)})"
      case PropertyAccess(base, property) => s"${p(base)}.$property"
      case UndefinedLiteral() => "undefined"
    }
  }

  private def printBlk(block: Block, indent: String, root: Boolean = false) = block
    .map(printNode(_, indent + (if(root) "" else "  "))) match {
      case l if root => l.mkString(";\n")
      case l => l.mkString("{\n", ";\n", "\n" + indent + "}")
    }

}


object LangTests {

  def main(args: Array[String]): Unit = {
    LangParser.script.parse("var x = $(x) { return undefined }; x()+ 1 + y.e.x()(); if (test) {@{a: l + b, b: ha.u(3)}}") match {
      case Parsed.Success(ast, _) =>
        println(ast)
        println(LangPrinter.print(ast))
      case f@Parsed.Failure(lastParser, _, extra) =>
        println(f)
    }
  }
}