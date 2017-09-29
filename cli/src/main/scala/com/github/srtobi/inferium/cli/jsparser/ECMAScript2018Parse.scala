package com.github.srtobi.inferium.cli.jsparser

import com.github.srtobi.inferium.cli.jsparser.Ast.{BlockStatement, EmptyStatement, VariableStatement}
import fastparse.all
import fastparse.all._
import fastparse.parsers.Terminals

import scala.collection.mutable

object ECMAScript2018TokensParser {
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
  val whiteSpace: UnitP = P("\t" | "\u000B" | "\u000C" | " " | "\u00A0" | zwnbsp /* | Other category "Zs" ???*/)

  // # 11.3
  val lineTerminator: UnitP = P("\n" | "\r" | "\u2028" | "\u2029")
  val lineTerminatorSequence: UnitP = P("\n" | "\r\n" | "\r" | "\f")

  // # 11.4
  val singleLineComment: UnitP = "//" ~ (!lineTerminator ~ sourceCharacter).rep
  private val commentEnd = "*/"
  val multiLineComment: UnitP = "/*" ~/ (!commentEnd ~ AnyChar).rep ~/ commentEnd
  val comment: UnitP = multiLineComment | singleLineComment

  // # from 11.8.4
  lazy val hex4Digits: UnitP = hexDigit.rep(exactly = 4)
  lazy val codePoint: UnitP = "{" ~ hexDigit.rep(max = 4) ~ "}"
  val unicodeEscapeSequence: UnitP = P("u" ~ (hex4Digits | codePoint))

  // # 11.6
  val identifierStart: UnitP = P(unicodeIdStart | "$" | "_" | "\\" ~ unicodeEscapeSequence)
  val identifierPart: UnitP = P(unicodeIdContinue | "$" | "_" | "\\" ~ unicodeEscapeSequence | zwnj | zwj)
  val identifierName: Parser[String] = P(unicodeIdStart ~ unicodeIdStart.rep).!

  // # 11.8.1 / 11.8.2
  val nullLiteral: UnitP = P("null")
  val booleanLiteral: UnitP = P("true" | "false")

  // # 11.8.3
  val hexDigit: UnitP = P(CharIn('0' to '9', 'a' to 'f', 'A' to 'F'))
  val octalDigit: UnitP = P(CharIn('0' to '7'))
  val binaryDigit: UnitP = P(CharIn('0' to '1'))


  //val commonToken: UnitP = identifierName
}

object Ast {

  sealed abstract class AstNode

  sealed case class Block(statement: Seq[Statement]) extends AstNode

  sealed abstract class Statement extends AstNode

  sealed abstract class NonDeclarationStatement extends Statement

  sealed case class EmptyStatement() extends Statement

  sealed case class BlockStatement(block: Block) extends Statement

  sealed case class VariableStatement(decls: Seq[Ast.VariableDeclaration]) extends Statement

  sealed case class IfStatement(condition: Expression, success: Statement, fail: Option[Statement]) extends Statement

  sealed case class DeclarationStatement(decl: Declaration) extends Statement

  sealed abstract class Binding extends AstNode
  sealed case class BindingPattern() extends Binding
  sealed case class BindingIdentifier(identifier: String) extends Binding

  sealed abstract class Declaration extends Statement

  sealed abstract class HoistableDeclaration extends Declaration

  sealed abstract class VariableDeclaration extends AstNode

  sealed case class IdentifierDeclaration(identifier: BindingIdentifier, initializer: Option[AssignmentExpression]) extends VariableDeclaration

  sealed case class PatternDeclaration(pattern: BindingPattern, initializer: AssignmentExpression) extends VariableDeclaration

  sealed abstract class Expression extends AstNode

  sealed abstract class AssignmentExpression extends Expression

  sealed case class Script(statements: Seq[Statement]) extends AstNode

  sealed class ClassDeclaration extends Declaration

  sealed class LexicalDeclaration extends Declaration

  case class DebuggerStatement() extends Statement

  case class CatchBlock(parameter: Binding, block: Block)
  case class TryStatement(block: Block, catchBlock: Option[CatchBlock], finallyBlock: Option[Block]) extends Statement

  sealed case class LabelledStatement(label: String, statement: Statement) extends Statement

  sealed case class FunctionDeclaration() extends Declaration

  case class SwitchStatement(expression: Expression, clauses: Seq[CaseClause]) extends Statement

  case class CaseClause(expression: Option[Expression], statements: Seq[Statement]) extends AstNode

  case class WithStatement(expression: Expression, statement: Statement) extends Statement

}

class ArgP[Arg, R](func: (Arg) => Parser[R]) {
  private val cache = mutable.HashMap.empty[Arg, Parser[R]]

  def apply(args: Arg): Parser[R] = cache.getOrElseUpdate(args, func(args))
}

object ArgP {
  def apply[Arg, R]()(func: (Arg) => Parser[R]): ArgP[Arg, R] = new ArgP[Arg, R](func)
}


object ECMAScript2018Parse {

  import ECMAScript2018TokensParser._

  // yield, await, return
  type YAR = (Boolean, Boolean, Boolean)
  // yield, await
  type YA = (Boolean, Boolean)
  // In, yield, await
  type IYA = (Boolean, Boolean, Boolean)
  // yield, await, default
  type YAD = (Boolean, Boolean, Boolean)


  // not implemented
  lazy val bindingIdentifier: Parser[Ast.BindingIdentifier] = identifierName.map(Ast.BindingIdentifier)
  lazy val bindingPattern: ArgP[YA, Ast.BindingPattern] = null
  lazy val initializer: ArgP[IYA, Ast.AssignmentExpression] = null
  lazy val expression: ArgP[IYA, Ast.Expression] = null
  lazy val labelIdentifier: ArgP[YA, String] = ArgP()(_ => P(identifierName)) // TODO: fix reserved keywords
  lazy val functionDeclaration: ArgP[YAD, Ast.FunctionDeclaration] =null

  // # 13.2
  lazy val blockStatement: ArgP[YAR, Ast.BlockStatement] = ArgP() {
    yar => block(yar).map(Ast.BlockStatement)
  }
  lazy val block: ArgP[YAR, Ast.Block] = ArgP() {
    yar => ("{" ~ statementList(yar) ~ "}").map(Ast.Block)
  }
  lazy val statementListItem: ArgP[YAR, Ast.Statement] = ArgP() {
    case yar@(y, a, _) => statement(yar) | declaration(y, a)
  }
  lazy val statementList: ArgP[YAR, Seq[Ast.Statement]] = ArgP() {
    yar => statementListItem(yar).rep(1)
  }

  // # 13.3.2
  lazy val variableStatement: ArgP[YA, Ast.VariableStatement] = ArgP() {
    case (y, a) => ("var" ~ variableDeclarationList(true, y, a) ~ ";").map(VariableStatement)
  }

  lazy val variableDeclarationList: ArgP[IYA, Seq[Ast.VariableDeclaration]] = ArgP() {
    iya => variableDeclaration(iya).rep(1)
  }

  lazy val variableDeclaration: ArgP[IYA, Ast.VariableDeclaration] = ArgP() {
    case iya@(_, y, a) =>
      P(bindingIdentifier ~ initializer(iya).?).map((Ast.IdentifierDeclaration _).tupled) |
        P(bindingPattern(y, a) ~ initializer(iya)).map((Ast.PatternDeclaration _).tupled)
  }

  // # 13.4
  lazy val emptyStatement: Parser[Ast.EmptyStatement] = P(";").map(_ => Ast.EmptyStatement())

  // # 13.6
  lazy val ifStatement: ArgP[YAR, Ast.IfStatement] = ArgP() {
    case yar@(y, a, _) => ("if" ~ "(" ~ expression(true, y, a) ~ ")" ~ statement(yar) ~ ("else" ~ statement(yar)).?).map((Ast.IfStatement _).tupled)
  }

  // 13.11
  lazy val withStatement: ArgP[YAR, Ast.WithStatement] = ArgP() {
    case yar@(y, a, _) => ("with" ~ "(" ~ expression(true, y, a) ~ ")" ~ statement(yar)).map((Ast.WithStatement _).tupled)
  }

  // 13.12
  lazy val switchStatement: ArgP[YAR, Ast.SwitchStatement] = ArgP() {
    case yar@(y, a, _) => ("switch" ~ "(" ~ expression(true, y, a) ~ ")" ~ caseBlock(yar)).map((Ast.SwitchStatement _).tupled)
  }
  lazy val caseBlock: ArgP[YAR, Seq[Ast.CaseClause]] = ArgP() {
    yar => (caseClauses(yar) ~ defaultClause(yar).? ~ caseClauses(yar)).map {
      case (cs1, c, cs2) => cs1 ++ c ++ cs2
    }
  }
  lazy val caseClauses: ArgP[YAR, Seq[Ast.CaseClause]] = ArgP()(caseClause(_).rep)
  lazy val caseClause: ArgP[YAR, Ast.CaseClause] = ArgP() {
    case yar@(y, a, _) => P("case" ~ expression(true, y, a).map(Some(_)) ~ ":" ~ statementList(yar)).map((Ast.CaseClause _).tupled)
  }
  lazy val defaultClause: ArgP[YAR, Ast.CaseClause] = ArgP() {
    yar => P("default" ~ ":" ~ statementList(yar)).map(Ast.CaseClause(None, _))
  }

  // 13.13
  lazy val labelledStatement: ArgP[YAR, Ast.LabelledStatement] = ArgP() {
    case yar@(y, a, _) => P(labelIdentifier(y, a) ~ ":" ~ labelledItem(yar)).map((Ast.LabelledStatement _).tupled)
  }
  lazy val labelledItem: ArgP[YAR, Ast.Statement] = ArgP() {
    case yar@(y, a, _) => statement(yar) | functionDeclaration(y, a, false)
  }

  // # 13.15
  lazy val tryStatement: ArgP[YAR, Ast.TryStatement] = ArgP() {
    yar => P("try" ~ block(yar) ~ `catch`(yar).? ~ `finally`(yar).?).map((Ast.TryStatement _).tupled)
  }
  lazy val `catch`: ArgP[YAR, Ast.CatchBlock] = ArgP() {
    case yar@(y, a, _) => P("catch" ~ "(" ~ catchParameter(y, a) ~ ")" ~ block(yar)).map((Ast.CatchBlock _).tupled)
  }
  lazy val `finally`: ArgP[YAR, Ast.Block] = ArgP() {
    yar => P("finally" ~ block(yar))
  }
  lazy val catchParameter: ArgP[YA, Ast.Binding] = ArgP() {
    ya => bindingIdentifier | bindingPattern(ya)
  }

  // # 13.16
  lazy val debuggerStatement: Parser[Ast.DebuggerStatement] = P("debugger" ~ ";").map(_ => Ast.DebuggerStatement())

  // # 13
  lazy val statement: ArgP[YAR, Ast.Statement] = ArgP() {
    case yar@(y, a, r) =>
      blockStatement(yar) |
        variableStatement(y, a) |
        emptyStatement |
        withStatement(yar) |
        switchStatement(yar) |
        labelledStatement(yar) |
        ifStatement(yar) |
        debuggerStatement
  }
  lazy val declaration: ArgP[YA, Ast.Declaration] = ArgP() {
    case (y, a) =>
      hoistableDeclaration(y, a, false) |
        classDeclaration(y, a, false) |
        lexicalDeclaration(true, y, a)
  }
  lazy val hoistableDeclaration: ArgP[YAD, Ast.HoistableDeclaration] = null
  lazy val classDeclaration: ArgP[YAD, Ast.ClassDeclaration] = null
  lazy val lexicalDeclaration: ArgP[IYA, Ast.LexicalDeclaration] = null


  // # 15.1
  lazy val script: Parser[Ast.Script] = statementList(false, false, false).map(Ast.Script)
}