package com.github.srtobi.inferium.cli.jsparser

import com.github.srtobi.inferium.cli.jsparser.Ast.VariableDeclarationType.VariableDeclarationType
import com.sun.xml.internal.ws.api.ComponentFeature.Target
import fastparse.parsers.{Combinators, Terminals}

import scala.collection.mutable
import fastparse.{all, core, noApi}
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
  val decimalIntegerLiteral: UnitP = "0" | nonZeroDigit ~ decimalDigits
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

  sealed abstract class PatternBinding extends Binding

  sealed case class ObjectPatternBinding(properties: Seq[PropertyBinding]) extends PatternBinding

  sealed trait PropertyBinding extends AstNode

  sealed case class PatternBindingProperty(property: PropertyName, element: BindingElement) extends PropertyBinding

  sealed case class ArrayPatternBinding(elements: Seq[ArrayElementBinding], rest: Option[Binding]) extends PatternBinding


  sealed trait ArrayElementBinding extends AstNode

  sealed case class ElisionBinding() extends ArrayElementBinding

  sealed trait BindingElement

  sealed case class PatternElementBinding(pattern: PatternBinding, default: Option[Expression]) extends ArrayElementBinding with BindingElement

  sealed case class SingleNameBinding(name: String, default: Option[Expression]) extends ArrayElementBinding with PropertyBinding with BindingElement

  sealed abstract class PropertyName extends AstNode
  sealed case class ComputedPropertyName(expression: Expression) extends PropertyName
  sealed case class LiteralPropertyName(literal: String) extends PropertyName

  sealed case class IdentifierBinding(identifier: String) extends Binding

  sealed abstract class Declaration extends AstNode

  sealed abstract class HoistableDeclaration extends Declaration

  sealed abstract class VariableDeclaration extends AstNode

  sealed case class IdentifierDeclaration(identifier: IdentifierBinding, initializer: Option[Expression]) extends VariableDeclaration

  sealed case class PatternDeclaration(pattern: PatternBinding, initializer: Expression) extends VariableDeclaration

  sealed abstract class Expression extends AstNode

  sealed abstract class AssignmentExpression extends Expression

  sealed case class Script(statements: Seq[Statement]) extends AstNode

  sealed class ClassDeclaration extends Declaration

  sealed class LexicalDeclaration extends Declaration

  case class DebuggerStatement() extends Statement

  case class CatchBlock(parameter: Binding, block: Block)

  case class TryStatement(block: Block, catchBlock: Option[CatchBlock], finallyBlock: Option[Block]) extends Statement

  sealed case class LabelledStatement(label: String, statement: Statement) extends Statement

  sealed case class FunctionDeclaration(function: Function) extends HoistableDeclaration

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

  sealed abstract class PrimaryExpression extends Expression

  sealed case class ThisExpression() extends PrimaryExpression

  sealed case class BinaryOperatorExpression(operator: String, left: Expression, right: Expression) extends Expression
  sealed case class ConditionalOperatior(condition: Expression, success: Expression, fail: Expression) extends Expression
  sealed case class UnaryExpression(operator: String, expression: Expression) extends Expression

  sealed case class Arguments(arguments: Seq[Expression], rest: Option[Expression]) extends AstNode

  sealed case class TemplateLiteral() extends AstNode

  sealed case class NewTargetExpression() extends Expression
  sealed case class SuperExpression() extends Expression
  sealed case class NewExpression(target: Expression, arguments: Option[Arguments]) extends Expression
  sealed abstract class CallExpression extends Expression
  sealed case class FunctionCallExpression(target: Expression, arguments: Arguments) extends CallExpression
  sealed case class ArrayAccessExpression(target: Expression, index: Expression) extends CallExpression
  sealed case class PropertyAccessExpression(target: Expression, property: String) extends CallExpression
  sealed case class TaggedTemplateExpression(tag: Expression, template: TemplateLiteral) extends CallExpression // TODO: do template

  sealed case class IdentifierReferenceExpression(identifier: String) extends PrimaryExpression
  sealed abstract class LiteralExpression extends PrimaryExpression
  sealed case class NullLiteral() extends LiteralExpression
  sealed case class BooleanLiteral(value: Boolean) extends LiteralExpression
  sealed case class NumericLiteral(value: String) extends LiteralExpression
  sealed case class StringLiteral(string: String) extends LiteralExpression
  sealed case class ArrayLiteral(elements: Seq[ArrayLiteralElement]) extends LiteralExpression
  sealed case class ObjectLiteral(properties: Seq[PropertyDefinition]) extends LiteralExpression

  sealed abstract class ArrayLiteralElement extends AstNode
  sealed case class ArrayElisionElement() extends ArrayLiteralElement
  sealed case class ArraySpreadElement(expression: Expression) extends ArrayLiteralElement
  sealed case class ArrayElement(expression: Expression) extends ArrayLiteralElement

  sealed abstract class PropertyDefinition extends AstNode
  sealed case class ShortcutPropertyDefinition(name: String) extends PropertyDefinition
  sealed case class NormalPropertyDefinition(name: PropertyName, initializer: Expression) extends PropertyDefinition
  sealed case class MethodPropertyDefinition() extends PropertyDefinition

  sealed case class Function(identifier: Option[String], parameters: Seq[BindingElement], rest: Option[Binding], body: Seq[Statement]) extends PrimaryExpression
}

private class JsWsWrapper(WL: P0) {
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
    Sequence.flatten(Sequence(p0, p, cut = true).asInstanceOf[Sequence[R, R, R, Char, String]])
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
  // In, await
  type IA = (Boolean, Boolean)
  // yield
  type Y = Boolean
  // yield, await, tagged
  type YAT = (Boolean, Boolean, Boolean)

  // # 11.9 Automatic Semicolon
  private lazy val `;` = noLineTerminator.rep ~~ P(";" | wsWithLineTerminator | &("}") | &(End))
  private lazy val noLineTerminatorHere = noLineTerminator.rep

  private def activate[V](a: Boolean, v: V): Option[V] = if (a) Some(v) else None

  private def toEither[T](l: (Boolean, Parser[T])*) = Combinators.Either(l.flatMap { case (a, v) => activate(a, v) }: _*)

  private def makeBinaryOperatorFolder(leftAssociative: Boolean) = {

    def leftFolder(acc: Ast.Expression, e: (String, Ast.Expression)) = e match {
      case (op, sub) => Ast.BinaryOperatorExpression(op, acc, sub)
    }
    def rightFolder(e: (String, Ast.Expression), acc: Ast.Expression) = leftFolder(acc, e)

    val fold = if (leftAssociative)
      (init: Ast.Expression, list: Seq[(String, Ast.Expression)]) => list.foldLeft(init)(leftFolder)
    else
      (init: Ast.Expression, list: Seq[(String, Ast.Expression)]) => list.foldRight(init)(rightFolder)

    fold
  }

  private def makeBinaryOperatorParser[Args, SubArgs, E <: Ast.Expression](opParser: Parser[String], subParser: ArgP[SubArgs, E], leftAssociative: Boolean, c: Args => SubArgs) = {
    val fold = makeBinaryOperatorFolder(leftAssociative)
    ArgP() {
      args: Args =>
        P(subParser(c(args)) ~ (opParser.! ~ subParser(c(args))).rep).map {
          case (init, list) => fold(init, list)
        }
    }
  }

  // not implemented

  lazy val yieldExpression: ArgP[IA, Ast.Expression] = ???
  lazy val arrowFunctionExpression: ArgP[IYA, Ast.Expression] = ???
  lazy val asyncArrowFunction: ArgP[IYA, Ast.Expression] = ???
  lazy val awaitExpression: ArgP[Y, Ast.Expression] = ???
  lazy val coverCallExpressionAndAsyncArrowHead: ArgP[YA, Ast.Expression] = ???
  lazy val templateLiteral: ArgP[YAT, Ast.TemplateLiteral] = ???
  lazy val methodDefiniton: ArgP[YA, Unit] = ???


  // 12.1
  lazy val identifierReference: ArgP[YA, String] = ArgP() {
    case (y, a) => toEither((true, identifier), (y, P("yield").!), (a, P("await").!))
  }
  lazy val bindingIdentifier: Parser[Ast.IdentifierBinding] = identifier.map(Ast.IdentifierBinding)
  lazy val labelIdentifier: ArgP[YA, String] = identifierReference
  lazy val identifier: Parser[String] = identifierName

  // # 12.2
  lazy val primaryExpression: ArgP[YA, Ast.PrimaryExpression] = ArgP() {
    ya => thisExpression |
      identifierReference(ya).map(Ast.IdentifierReferenceExpression) |
      literal |
      arrayLiteral(ya) |
      objectLiteral(ya) |
      functionExpression(ya)
  }

  // 12.2.2
  lazy val thisExpression: Parser[Ast.ThisExpression] = P("this").map(_ => Ast.ThisExpression())

  // 12.2.4
  lazy val literal: Parser[Ast.LiteralExpression] = P(
    nullLiteral.map(_ => Ast.NullLiteral()) |
    booleanLiteral.map(Ast.BooleanLiteral) |
    numericLiteral.!.map(Ast.NumericLiteral) |
    stringLiteral.map(Ast.StringLiteral)
  )

  // 12.2.5
  lazy val arrayLiteral: ArgP[YA, Ast.ArrayLiteral] = ArgP() {
    ya => P("[" ~ elementList(ya) ~ "]").map(Ast.ArrayLiteral)
  }
  lazy val elementList: ArgP[YA, Seq[Ast.ArrayLiteralElement]] = ArgP() {
    case ya@(y, a) => (
      P(",").map(_ => Ast.ArrayElisionElement()) |
      assignmentExpression(true, y, a).map(Ast.ArrayElement) |
      spreadElement(ya)
    ).rep(sep=",")
  }
  lazy val spreadElement: ArgP[YA, Ast.ArraySpreadElement] = ArgP() {
    case (y, a) => P("..." ~ assignmentExpression(true, y, a)).map(Ast.ArraySpreadElement)
  }

  // 12.2.6
  lazy val objectLiteral: ArgP[YA, Ast.ObjectLiteral] = ArgP() {
    ya => P("{" ~ propertyDefinitionList(ya) ~ ",".? ~ "}").map(Ast.ObjectLiteral)
  }
  lazy val propertyDefinitionList: ArgP[YA, Seq[Ast.PropertyDefinition]] = ArgP() {
    ya => propertyDefinition(ya).rep(sep=",")
  }
  lazy val propertyDefinition: ArgP[YA, Ast.PropertyDefinition] = ArgP() {
    case ya@(y, a) =>
      identifierReference(ya).map(Ast.ShortcutPropertyDefinition) |
      // TODO: CoverInitializedName is ignored... I am not sure in which context this would be allowed
      P(propertyName(ya) ~ ":" ~ assignmentExpression(true, y, a)).map((Ast.NormalPropertyDefinition.apply _).tupled) |
      methodDefiniton(ya).map(_ => Ast.MethodPropertyDefinition()) // TODO: implement it
  }
  lazy val propertyName: ArgP[YA, Ast.PropertyName] = ArgP() { _ => P(Fail) }
  lazy val literalPropertyName: Parser[Ast.PropertyName] = P(
    identifierName |
    stringLiteral |
    numericLiteral
  ).!.map(Ast.LiteralPropertyName)
  lazy val computedPropertyName: ArgP[YA, Ast.PropertyName] = ArgP() {
    case (y, a) => P("[" ~ assignmentExpression(true, y, a) ~ "]").map(Ast.ComputedPropertyName)
  }
  lazy val initializer: ArgP[IYA, Ast.Expression] = ArgP() {
    iya => "=" ~ assignmentExpression(iya)
  }

  // 12.3
  private def makeTargetExpression(ya: (Boolean, Boolean))( target: Ast.Expression) = ya match {
    case (y, a) =>
      arguments(ya).map(Ast.FunctionCallExpression(target, _)) |
        P("[" ~ expression(true, y, a) ~ "]").map(Ast.ArrayAccessExpression(target, _)) |
        P("." ~ identifierName).map(Ast.PropertyAccessExpression(target, _)) |
        P(templateLiteral(y, a, true)).map(Ast.TaggedTemplateExpression(target, _))
  }
  lazy val memberExpression: ArgP[YA, Ast.Expression] = ArgP() {
    case ya@(y, a) =>
      P(
        primaryExpression(ya) |
        superProperty(ya) |
        metaProperty |
        ("new" ~ memberExpression(ya) ~ arguments(ya)).map{ case (target, args) => Ast.NewExpression(target, Some(args))}
      ).flatMap(makeTargetExpression(ya))
  }
  lazy val superProperty: ArgP[YA, Ast.Expression] = ArgP() {
    case (y, a) => "super" ~ (
      P("[" ~ expression(true, y, a) ~ "]").map(Ast.ArrayAccessExpression(Ast.SuperExpression(), _)) |
      P("." ~ identifierName).map(Ast.PropertyAccessExpression(Ast.SuperExpression(), _))
    )
  }
  lazy val metaProperty: Parser[Ast.NewTargetExpression] = newTarget
  lazy val newTarget: Parser[Ast.NewTargetExpression] = P("new" ~ "." ~ "target").map(_ => Ast.NewTargetExpression())
  lazy val newExpression: ArgP[YA, Ast.Expression] = ArgP() {
    ya => memberExpression(ya) | ("new" ~ newExpression(ya)).map(Ast.NewExpression(_, None))
  }
  lazy val callExpression: ArgP[YA, Ast.Expression] = ArgP() {
    case ya@(y, a) =>
      P(
        coverCallExpressionAndAsyncArrowHead(ya) |
        superCall(ya)
      ).flatMap(makeTargetExpression(ya))
  }
  lazy val superCall: ArgP[YA, Ast.Expression] = ArgP() {
    ya => P("super" ~ arguments(ya)).map(args => Ast.FunctionCallExpression(Ast.SuperExpression(), args))
  }
  lazy val arguments: ArgP[YA, Ast.Arguments] = ArgP() {
    ya => "(" ~ argumentList(ya) ~ ",".? ~ ")"
  }
  lazy val argumentList: ArgP[YA, Ast.Arguments] = ArgP() {
    case (y, a) =>
      P(
        assignmentExpression(true, y, a).rep(sep = P(",")) ~
        ("," ~ "..." ~ assignmentExpression(true, y, a)).?
      ).map((Ast.Arguments.apply _).tupled)
  }
  lazy val leftHandSideExpression: ArgP[YA, Ast.Expression] = ArgP() {
    ya => newExpression(ya) | callExpression(ya)
  }

  // 12.4
  lazy val updateExpression: ArgP[YA, Ast.Expression] = ArgP() {
    ya =>
      P(  // TODO: use better operator representation
        StringIn("++", "--").!.map("pre_" + _) ~ leftHandSideExpression(ya) |
        P(leftHandSideExpression(ya) ~~ noLineTerminatorHere ~~ StringIn("++", "--").!.map("post_" + _))
          .map{case (e, op) => (op, e)}
      ).map((Ast.UnaryExpression.apply _).tupled)
  }
  // 12.5
  lazy val unaryExpression: ArgP[YA, Ast.Expression] = ArgP() {
    case ya@(y, a) =>
      toEither((true, updateExpression(ya)), (true, unaryOperatorExpression(ya)), (a, awaitExpression(y)))
  }
  private lazy val unaryOperatorExpression: ArgP[YA, Ast.UnaryExpression] = ArgP() {
    ya => P(unaryOperators ~ unaryExpression(ya)).map((Ast.UnaryExpression.apply _).tupled)
  }
  private val unaryOperators = StringIn("delete", "void", "typeof", "+", "-", "~", "!").!

  // 12.6
  lazy val exponentiationExpression: ArgP[YA, Ast.Expression] = makeBinaryOperatorParser(P("**").!, additiveExpression, leftAssociative = false, a => a)

  // 12.7
  lazy val multiplicativeExpression: ArgP[YA, Ast.Expression] = makeBinaryOperatorParser(multiplicativeOperators, exponentiationExpression, leftAssociative = true, a => a)
  private val multiplicativeOperators = StringIn("*", "/", "%").!

  // 12.8
  lazy val additiveExpression: ArgP[YA, Ast.Expression] = makeBinaryOperatorParser(additiveOperators, multiplicativeExpression, leftAssociative = true, a => a)
  private val additiveOperators = StringIn("+", "-").!

  // 12.9
  lazy val shiftExpression: ArgP[YA, Ast.Expression] = makeBinaryOperatorParser(shiftOperator, additiveExpression, leftAssociative = true, a => a)
  private val shiftOperator = StringIn(">>>", ">>", "<<").!
  // 12.10
  // NOTE: the in-production would give +In to it's subsequent RelationalExpressions
  //       this is not explicitly stated here, because the production is only used if i is already true
  lazy val relationalExpression: ArgP[IYA, Ast.Expression] = ArgP() {
    case iya@(i, _, _) => makeBinaryOperatorParser(relationalOperator(i), shiftExpression, leftAssociative = true, (t:IYA) => t match {case (_, y, a) => (y, a)})(iya)
  }
  private def relationalOperator(in: Boolean) = StringIn(Seq("<=", ">=", "<", ">", "instanceof") ++ (if (in) Seq("in") else Seq()): _*).!

  // 12.11
  lazy val equalityExpression: ArgP[IYA, Ast.Expression] = makeBinaryOperatorParser(equalityOperator, relationalExpression, leftAssociative = true, a => a)
  private val equalityOperator = StringIn("!==", "===", "!=", "==").!

  // 12.12
  lazy val bitwiseANDExpression: ArgP[IYA, Ast.Expression] = makeBinaryOperatorParser(P("&").!, equalityExpression, leftAssociative = true, a => a)
  lazy val bitwiseXORExpression: ArgP[IYA, Ast.Expression] = makeBinaryOperatorParser(P("^").!, bitwiseANDExpression, leftAssociative = true, a => a)
  lazy val bitwiseORExpression: ArgP[IYA, Ast.Expression] = makeBinaryOperatorParser(P("|").!, bitwiseXORExpression, leftAssociative = true, a => a)

  // 12.13
  lazy val logicalANDExpression: ArgP[IYA, Ast.Expression] = makeBinaryOperatorParser(P("&&").!, bitwiseORExpression, leftAssociative = true, a => a)
  lazy val logicalORExpression: ArgP[IYA, Ast.Expression] = makeBinaryOperatorParser(P("||").!, logicalANDExpression, leftAssociative = true, a => a)

  // 12.14
  lazy val conditionalExpression: ArgP[IYA, Ast.Expression] = ArgP() {
    case (iya) => P(logicalORExpression(iya) ~ ("?" ~/ assignmentExpression(iya) ~ ":" ~/ assignmentExpression(iya)).?).map {
      case (expr, None) => expr
      case (cond, Some((success, fail))) => Ast.ConditionalOperatior(cond, success, fail)
    }
  }


  // 12.15
  lazy val assignmentExpression: ArgP[IYA, Ast.Expression] = ArgP() {
    case iya@(i, y, a) =>
      toEither(
        (true, NoCut(conditionalExpression(iya))),
        (y, yieldExpression(i, a)),
        (true, arrowFunctionExpression(iya)),
        (true, asyncArrowFunction(iya)),
        (true, (leftHandSideExpression(i, a) ~ assignmentOperator ~ assignmentExpression(iya)).map{case (l, op, r) => Ast.BinaryOperatorExpression(op, l, r)})
      )
  }
  lazy val assignmentOperator: Parser[String] = StringIn("=", "*=", "/=", "%=", "+=" , "-=", "<<=", ">>=", ">>>=", "&=", "^=", "|=", "**=").!

  // 12.16
  lazy val expression: ArgP[IYA, Ast.Expression] = makeBinaryOperatorParser(P(",").!, primaryExpression, leftAssociative = true, _ => (true, true))

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
  lazy val bindingPattern: ArgP[YA, Ast.PatternBinding] = ArgP() {
    ya => objectBindingPattern(ya) | arrayBindingPattern(ya)
  }
  lazy val objectBindingPattern: ArgP[YA, Ast.ObjectPatternBinding] = ArgP() {
    ya => P("{" ~/ bindingPropertyList(ya) ~/ ",".? ~ "}").map(Ast.ObjectPatternBinding)
  }
  lazy val arrayBindingPattern: ArgP[YA, Ast.ArrayPatternBinding] = ArgP() {
    ya =>
      P("[" ~/
        (bindingElement(ya) | P("").map(_ => Ast.ElisionBinding())).rep(sep = ",") ~
        bindingRestElement(ya).? ~/
        "]"
      ).map((Ast.ArrayPatternBinding.apply _).tupled)
  }
  lazy val bindingPropertyList: ArgP[YA, Seq[Ast.PropertyBinding]] = ArgP() {
    ya => bindingProperty(ya).rep(sep = ",")
  }
  lazy val bindingProperty: ArgP[YA, Ast.PropertyBinding] = ArgP() {
    ya =>
      singleNameBinding(ya) |
        P(propertyName(ya) ~ ":" ~ bindingElement(ya)).map((Ast.PatternBindingProperty.apply _).tupled)
  }
  lazy val bindingElement: ArgP[YA, Ast.ArrayElementBinding with Ast.BindingElement] = ArgP() {
    case ya@(y, a) => singleNameBinding(ya) | (bindingPattern(ya) ~ initializer(true, y, a).?).map((Ast.PatternElementBinding.apply _).tupled)
  }
  lazy val singleNameBinding: ArgP[YA, Ast.SingleNameBinding] = ArgP() {
    case (y, a) => P(bindingIdentifier.map(_.identifier) ~ initializer(true, y, a).?).map((Ast.SingleNameBinding.apply _).tupled)
  }
  lazy val bindingRestElement: ArgP[YA, Ast.Binding] = ArgP() {
    ya => "..." ~ (bindingIdentifier | bindingPattern(ya))
  }

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
        (if(r) returnStatement(y, a) else Fail) | //TODO: Custom error
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
    case (y, a, d) => P(
      "function" ~/
        bindingIdentifier.map(id => Some(id.identifier)) ~/
        "(" ~ formalParameters(y, a) ~ ")" ~
        "{" ~ functionBody(y, a) ~ "}"
    ).map { case (id, (params, rest), body) => Ast.Function(id, params, rest, body) }
      .map(Ast.FunctionDeclaration)
  }
  lazy val functionExpression: ArgP[YA, Ast.Function] = ArgP() {
    ya => P(
      "function" ~/
        bindingIdentifier.map(_.identifier).? ~/
        "(" ~ formalParameters(ya) ~ ")" ~
        "{" ~ functionBody(ya) ~ "}"
    ).map { case (id, (params, rest), body) => Ast.Function(id, params, rest, body) }
  }
  lazy val uniqueFormalParameters: ArgP[YA, (Seq[Ast.BindingElement], Option[Ast.Binding])] = formalParameters
  lazy val formalParameters: ArgP[YA, (Seq[Ast.BindingElement], Option[Ast.Binding])] = ArgP() {
    ya => formalParameter(ya).rep(sep=",") ~ (P(",").map(_ => None) | functionRestParameter(ya).?)
  }
  lazy val formalParameterList: ArgP[YA, Seq[Ast.BindingElement]] = ArgP() {
    ya => formalParameter(ya).rep
  }
  lazy val functionRestParameter: ArgP[YA, Ast.Binding] = bindingRestElement
  lazy val formalParameter: ArgP[YA, Ast.ArrayElementBinding with Ast.BindingElement] = bindingElement
  lazy val functionBody: ArgP[YA, Seq[Ast.Statement]] = functionStatementList
  lazy val functionStatementList: ArgP[YA, Seq[Ast.Statement]] = ArgP() {
    case (y, a) => statementList(y, a, true)
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
    ECMAScript2018Parse.script.parse("return;") match {
      case Parsed.Success(ast, _) =>
        println(ast)
      case f@Parsed.Failure(lastParser, _, extra) =>
        println(f)
    }
  }
}
