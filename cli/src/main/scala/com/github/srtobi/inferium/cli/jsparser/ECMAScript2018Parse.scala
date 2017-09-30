package com.github.srtobi.inferium.cli.jsparser

import com.github.srtobi.inferium.cli.jsparser.Ast.VariableDeclarationType.VariableDeclarationType
import fastparse.parsers.{Combinators, Terminals}

import scala.collection.mutable
import fastparse.{all, noApi}
import fastparse.noApi._


class ArgP[Arg, R](func: (Arg) => Parser[R]) {
  private val cache = mutable.HashMap.empty[Arg, Parser[R]]

  def apply(args: Arg): Parser[R] = cache.getOrElseUpdate(args, func(args))
}

object ArgP {
  def apply[Arg, R]()(func: (Arg) => Parser[R]): ArgP[Arg, R] = new ArgP[Arg, R](func)
}

trait StringToSourceName {
  protected implicit def toSourceName(name: String): sourcecode.Name = sourcecode.Name(name)
}

object ECMAScript2018TokensParser extends StringToSourceName {

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
  val whiteSpace: UnitP = P(CharIn(" ", "\t", "\u000B", "\u000C", "\u00A0", zwnbsp /* | Other category "Zs" ???*/))("ws")

  // # 11.3
  val lineTerminator: UnitP = P(CharIn("\n", "\r", "\u2028", "\u2029"))("line-terminator")
  val lineTerminatorSequence: UnitP = P(StringIn("\n", "\r\n", "\r", "\u2028", "\u2029"))("\\n")

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
    )("multiline-comment")).map(_.getOrElse(false))
  val comment: UnitP = P(multiLineComment.map(_ => ()) | singleLineComment)("comment")
  val inlineComment: UnitP = multiLineComment.filter(!_).map(_ => ())

  // # from 11.8.4
  lazy val hex4Digits: UnitP = hexDigit.rep(exactly = 4)
  lazy val codePoint: UnitP = "{" ~ hexDigit.rep(max = 4) ~ "}"
  val unicodeEscapeSequence: UnitP = P("u" ~ (hex4Digits | codePoint))

  // # 11.6.2
  val futureReservedWord: Seq[String] = Seq("enum")
  val keyword: Seq[String] = Seq("await", "break", "case", "catch", "class", "const", "continue",
    "debugger", "default", "delete", "do", "else", "export", "extends", "finally", "for", "function",
    "if", "import", "in", "instanceof", "new", "return", "super", "switch", "this", "throw", "try", "typeof",
    "var", "void", "while", "with", "yield")
  val reservedWord: Seq[String] = keyword ++ Seq("null", "true", "false") ++ futureReservedWord

  // # 11.6
  val identifierStart: UnitP = P(unicodeIdStart | "$" | "_" | "\\" ~ unicodeEscapeSequence)
  val identifierPart: UnitP = P(unicodeIdContinue | "$" | "_" | "\\" ~ unicodeEscapeSequence | zwnj | zwj)
  val identifierName: Parser[String] = P(unicodeIdStart ~ unicodeIdStart.rep).!.filter(!reservedWord.contains(_))

  // # 11.8.1 / 11.8.2
  val nullLiteral: UnitP = P("null")
  val booleanLiteral: UnitP = StringIn("true", "false")

  // # 11.8.3
  val hexDigit: UnitP = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val octalDigit: UnitP = P(CharIn('0' to '7'))
  val binaryDigit: UnitP = P(CharIn('0' to '1'))

  //val commonToken: UnitP = identifierName
  val ws: UnitP = P(whiteSpace | lineTerminatorSequence | comment)("ws")
  val wsWithLineTerminator: UnitP = P(lineTerminatorSequence | multiLineComment.filter(a => a).map(_ => ()) | singleLineComment)("line-terminating")
  val noLineTerminator: UnitP = P(whiteSpace | inlineComment)("no-line-terminator")
}

object Ast {
  object VariableDeclarationType extends Enumeration {
    type VariableDeclarationType = Value
    val Var, Let, Const = Value
  }

  sealed abstract class AstNode

  sealed case class Block(statement: Seq[Statement]) extends AstNode

  sealed abstract class Statement extends AstNode

  sealed abstract class NonDeclarationStatement extends Statement

  sealed case class EmptyStatement() extends Statement

  sealed case class BlockStatement(block: Block) extends Statement

  sealed case class VariableStatement(declType: VariableDeclarationType, decls: Seq[Ast.VariableDeclaration]) extends Statement

  sealed case class IfStatement(condition: Expression, success: Statement, fail: Option[Statement]) extends Statement

  sealed case class DeclarationStatement(decl: Declaration) extends Statement

  sealed abstract class Binding extends AstNode

  sealed case class PatternBinding() extends Binding

  sealed case class IdentifierBinding(identifier: String) extends Binding

  sealed abstract class Declaration extends AstNode

  sealed abstract class HoistableDeclaration extends Declaration

  sealed abstract class VariableDeclaration extends AstNode

  sealed case class IdentifierDeclaration(identifier: IdentifierBinding, initializer: Option[AssignmentExpression]) extends VariableDeclaration

  sealed case class PatternDeclaration(pattern: PatternBinding, initializer: AssignmentExpression) extends VariableDeclaration

  sealed abstract class Expression extends AstNode

  sealed abstract class AssignmentExpression extends Expression

  sealed case class Script(statements: Seq[Statement]) extends AstNode

  sealed class ClassDeclaration extends Declaration

  sealed class LexicalDeclaration extends Declaration

  case class DebuggerStatement() extends Statement

  case class CatchBlock(parameter: Binding, block: Block)

  case class TryStatement(block: Block, catchBlock: Option[CatchBlock], finallyBlock: Option[Block]) extends Statement

  sealed case class LabelledStatement(label: String, statement: Statement) extends Statement

  sealed case class FunctionDeclaration() extends HoistableDeclaration
  sealed case class GeneratorDeclaration() extends HoistableDeclaration

  case class SwitchStatement(expression: Expression, clauses: Seq[CaseClause]) extends Statement

  case class CaseClause(expression: Option[Expression], statements: Seq[Statement]) extends AstNode

  case class WithStatement(expression: Expression, statement: Statement) extends Statement

  case class ReturnStatement(expression: Option[Expression]) extends Statement

  case class BreakStatement(label: Option[String]) extends Statement

  case class ContinueStatement(label: Option[String]) extends Statement

  /*sealed abstract class IterationStatement extends Statement

  case class DoWhileStatement(expression: Expression) extends IterationStatement

  case class WhileStatement(expression: Expression) extends IterationStatement

  abstract class ForInit
  abstract class ForStatement() extends IterationStatement

  abstract class Expr*/
}

private class JsWsWrapper(WL: P0){
  implicit def parserApi2[T, V](p0: T)(implicit c: T => P[V]): JsWhitespaceApi[V] =
    new JsWhitespaceApi[V](p0, WL)
}

private object JsWsApi extends JsWsWrapper({
  import fastparse.all._
  import ECMAScript2018TokensParser._
  ws.rep
})

private class JsWhitespaceApi[+T](p0: P[T], WL: P0) extends fastparse.WhitespaceApi[T](p0, WL) {
  import fastparse.all._
  import fastparse.parsers.Combinators.Sequence
  import fastparse.core.Implicits.Sequencer

  def ~~/[V, R](p: Parser[V])(implicit ev: Sequencer[T, V, R]): Parser[R] =
    Sequence.flatten(Sequence(p0, p, cut=true).asInstanceOf[Sequence[R, R, R, Char, String]])
}


object ECMAScript2018Parse extends StringToSourceName {

  import ECMAScript2018TokensParser._
  import JsWsApi._


  // yield, await, return
  type YAR = (Boolean, Boolean, Boolean)
  // yield, await
  type YA = (Boolean, Boolean)
  // In, yield, await
  type IYA = (Boolean, Boolean, Boolean)
  // yield, await, default
  type YAD = (Boolean, Boolean, Boolean)


  // # 11.9 Automatic Semicolon
  private lazy val `;` = noLineTerminator.rep ~~ P(";" | wsWithLineTerminator | &("}") | &(End))
  private lazy val noLineTerminatorHere = noLineTerminator.rep

  private def activate[V](a: Boolean, v: V): Option[V] = if (a) Some(v) else None
  private def toEither[T](l: (Boolean, Parser[T])*) = Combinators.Either(l.flatMap { case (a, v) => activate(a, v) }: _*)

  // not implemented
  lazy val bindingIdentifier: Parser[Ast.IdentifierBinding] = identifierName.map(Ast.IdentifierBinding)
  lazy val initializer: ArgP[IYA, Ast.AssignmentExpression] = ArgP() { _ => P(Fail) }
  lazy val expression: ArgP[IYA, Ast.Expression] = ArgP() { _ => P(Fail) }
  lazy val labelIdentifier: ArgP[YA, String] = ArgP() {
    case (y, a) => toEither((true, identifierName), (y, P("yield").!), (a, P("await").!))
  }

  // # 13.2
  lazy val blockStatement: ArgP[YAR, Ast.BlockStatement] = ArgP() {
    yar => block(yar).map(Ast.BlockStatement)
  }
  lazy val block: ArgP[YAR, Ast.Block] = ArgP() {
    yar => P("{" ~/ statementListOpt(yar) ~ "}").map(Ast.Block)
  }
  lazy val statementListItem: ArgP[YAR, Ast.Statement] = ArgP() {
    case yar@(y, a, _) => statement(yar) | declaration(y, a)
  }
  lazy val statementList: ArgP[YAR, Seq[Ast.Statement]] = ArgP() {
    yar => statementListItem(yar).rep(1)
  }
  lazy val statementListOpt: ArgP[YAR, Seq[Ast.Statement]] = ArgP() {
    yar => statementListItem(yar).rep
  }

  // # 13.3.1
  lazy val lexicalDeclaration: ArgP[IYA, Ast.VariableStatement] = ArgP() {
    iya => P(letOrConst ~/ bindingList(iya) ~~ `;`).map((Ast.VariableStatement.apply _).tupled)
  }
  lazy val letOrConst: Parser[Ast.VariableDeclarationType.VariableDeclarationType] = {
    P("let").map(_ => Ast.VariableDeclarationType.Let) |
      P("const").map(_ => Ast.VariableDeclarationType.Const)
  }

  lazy val bindingList: ArgP[IYA, Seq[Ast.VariableDeclaration]] = ArgP() {
    iya => lexicalBinding(iya).rep(1)
  }

  lazy val lexicalBinding: ArgP[IYA, Ast.VariableDeclaration] = ArgP() {
    case iya@(_, y, a) =>
      P(bindingIdentifier ~ initializer(iya).?).map((Ast.IdentifierDeclaration.apply _).tupled) |
        P(bindingPattern(y, a) ~ initializer(iya)).map((Ast.PatternDeclaration.apply _).tupled)
  }


  // # 13.3.2
  lazy val variableStatement: ArgP[YA, Ast.VariableStatement] = ArgP() {
    case (y, a) => ("var" ~/ variableDeclarationList(true, y, a) ~~ `;`).map(decls => Ast.VariableStatement(Ast.VariableDeclarationType.Var, decls))
  }

  lazy val variableDeclarationList: ArgP[IYA, Seq[Ast.VariableDeclaration]] = ArgP() {
    iya => variableDeclaration(iya).rep(1)
  }

  lazy val variableDeclaration: ArgP[IYA, Ast.VariableDeclaration] = ArgP() {
    case iya@(_, y, a) =>
      P(bindingIdentifier ~ initializer(iya).?).map((Ast.IdentifierDeclaration.apply _).tupled) |
        P(bindingPattern(y, a) ~ initializer(iya)).map((Ast.PatternDeclaration.apply _).tupled)
  }

  // # 13.3.3
  lazy val bindingPattern: ArgP[YA, Ast.PatternBinding] = ArgP() { _ => P(Fail) }

  // # 13.4
  lazy val emptyStatement: Parser[Ast.EmptyStatement] = P(";").map(_ => Ast.EmptyStatement())

  // # 13.6
  lazy val ifStatement: ArgP[YAR, Ast.IfStatement] = ArgP() {
    case yar@(y, a, _) => P("if" ~/ "(" ~ expression(true, y, a) ~ ")" ~ statement(yar) ~ ("else" ~ statement(yar)).?)("if-statement").map((Ast.IfStatement.apply _).tupled)
  }

  // # 13.7


  // # 13.8
  lazy val continueStatement: ArgP[YA, Ast.ContinueStatement] = ArgP() {
    case (y, a) => P("continue" ~~/ (noLineTerminatorHere ~~ labelIdentifier(y, a)).? ~~ `;`)("continue-statmeent").map(Ast.ContinueStatement)
  }

  // # 13.9
  lazy val breakStatement: ArgP[YA, Ast.BreakStatement] = ArgP() {
    case (y, a) => P("break" ~~/ (noLineTerminatorHere ~~ labelIdentifier(y, a)).? ~~ `;`)("break-statmeent").map(Ast.BreakStatement)
  }

  // # 13.10
  lazy val returnStatement: ArgP[YA, Ast.ReturnStatement] = ArgP() {
    case (y, a) => P("return" ~~/ noLineTerminatorHere ~~ expression(true, y, a).? ~~ `;`)("return-statmeent").map(Ast.ReturnStatement)
  }

  // 13.11
  lazy val withStatement: ArgP[YAR, Ast.WithStatement] = ArgP() {
    case yar@(y, a, _) => P("with" ~/ "(" ~ expression(true, y, a) ~ ")" ~ statement(yar))("with-statement").map((Ast.WithStatement.apply _).tupled)
  }

  // 13.12
  lazy val switchStatement: ArgP[YAR, Ast.SwitchStatement] = ArgP() {
    case yar@(y, a, _) => P("switch" ~/ "(" ~ expression(true, y, a) ~ ")" ~ caseBlock(yar))("switch-statement").map((Ast.SwitchStatement.apply _).tupled)
  }
  lazy val caseBlock: ArgP[YAR, Seq[Ast.CaseClause]] = ArgP() {
    yar =>
      (caseClausesOpt(yar) ~ defaultClause(yar).? ~ caseClausesOpt(yar)).map {
        case (cs1, c, cs2) => cs1 ++ c ++ cs2
      }
  }
  lazy val caseClausesOpt: ArgP[YAR, Seq[Ast.CaseClause]] = ArgP()(caseClause(_).rep)
  lazy val caseClause: ArgP[YAR, Ast.CaseClause] = ArgP() {
    case yar@(y, a, _) => P("case" ~/ expression(true, y, a).map(Some(_)) ~/ ":" ~/ statementListOpt(yar))("case-clause").map((Ast.CaseClause.apply _).tupled)
  }
  lazy val defaultClause: ArgP[YAR, Ast.CaseClause] = ArgP() {
    yar => P("default" ~/ ":" ~/ statementListOpt(yar))("default-clause").map(Ast.CaseClause(None, _))
  }

  // 13.13
  lazy val labelledStatement: ArgP[YAR, Ast.LabelledStatement] = ArgP() {
    case yar@(y, a, _) => P(labelIdentifier(y, a) ~ ":" ~ labelledItem(yar)).map((Ast.LabelledStatement.apply _).tupled)
  }
  lazy val labelledItem: ArgP[YAR, Ast.Statement] = ArgP() {
    case yar@(y, a, _) => statement(yar) | functionDeclaration(y, a, false).map(Ast.DeclarationStatement)
  }

  // # 13.15
  lazy val tryStatement: ArgP[YAR, Ast.TryStatement] = ArgP() {
    yar => P("try" ~/ block(yar) ~/ `catch`(yar).? ~ `finally`(yar).?).map((Ast.TryStatement.apply _).tupled)
  }
  lazy val `catch`: ArgP[YAR, Ast.CatchBlock] = ArgP() {
    case yar@(y, a, _) => P("catch" ~/ "(" ~/ catchParameter(y, a) ~ ")" ~ block(yar)).map((Ast.CatchBlock.apply _).tupled)
  }
  lazy val `finally`: ArgP[YAR, Ast.Block] = ArgP() {
    yar => P("finally" ~/ block(yar))
  }
  lazy val catchParameter: ArgP[YA, Ast.Binding] = ArgP() {
    ya => bindingIdentifier | bindingPattern(ya)
  }

  // # 13.16
  lazy val debuggerStatement: Parser[Ast.DebuggerStatement] = P("debugger" ~~ `;`)("debugger-statement").map(_ => Ast.DebuggerStatement())

  // # 13
  lazy val statement: ArgP[YAR, Ast.Statement] = ArgP() {
    case yar@(y, a, r) =>
      blockStatement(yar) |
        variableStatement(y, a) |
        emptyStatement |
        continueStatement(y, a) |
        breakStatement(y, a) |
        returnStatement(y, a) |
        withStatement(yar) |
        switchStatement(yar) |
        labelledStatement(yar) |
        ifStatement(yar) |
        debuggerStatement
  }
  lazy val declaration: ArgP[YA, Ast.Statement] = ArgP() {
    case (y, a) =>
      hoistableDeclaration(y, a, false).map(Ast.DeclarationStatement) |
        classDeclaration(y, a, false).map(Ast.DeclarationStatement) |
        lexicalDeclaration(true, y, a)
  }
  lazy val hoistableDeclaration: ArgP[YAD, Ast.HoistableDeclaration] = ArgP() {
    yad => functionDeclaration(yad) | generatorDeclaration(yad) | asyncFunctionDeclaration(yad)
  }
  lazy val classDeclaration: ArgP[YAD, Ast.ClassDeclaration] = ArgP() { _ => P(Fail) }


  // # 14.1
  lazy val functionDeclaration: ArgP[YAD, Ast.FunctionDeclaration] = ArgP() {
    _ => Fail
  }

  // # 14.6
  lazy val generatorDeclaration: ArgP[YAD, Ast.GeneratorDeclaration] = ArgP() {
    _ => Fail
  }

  // # 14.6
  lazy val asyncFunctionDeclaration: ArgP[YAD, Ast.FunctionDeclaration] = ArgP() {
    _ => Fail
  }

  // # 15.1
  lazy val script: Parser[Ast.Script] = P(ws.rep ~~ statementListOpt(false, false, false) ~ End)("script").map(Ast.Script)
}

object JsTests {

  def main(args: Array[String]): Unit = {
    val Parsed.Success(ast, _) = ECMAScript2018Parse.script.parse("const a;")
    println(ast)
  }
}