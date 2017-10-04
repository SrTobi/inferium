package com.github.srtobi.inferium.cli.jsparser

object Ast {

  object VariableDeclarationType extends Enumeration {
    type VariableDeclarationType = Value
    val Var, Let, Const = Value
  }

  object ClosureType extends Enumeration {
    type ClosureType = Value
    val Normal, Async, Generator, Arrow, AsyncArrow = Value
  }

  sealed abstract class Operator {
    def text: String
    def name: String
    override def toString: String = name
    override def equals(o: scala.Any): Boolean = o match {
      case other: Operator => name == other.name
      case _ => false
    }
  }
  sealed case class BinaryOperator(override val text: String, override val name: String) extends Operator {
    def this(text: String) = this(text, text)
  }
  sealed case class UnaryOperator(override val text: String, override val name: String) extends Operator {
    def this(text: String) = this(text, text)
  }

  object Operator {
    // Unary Operators
    object `post++` extends UnaryOperator("++", "post++")
    object `post--` extends UnaryOperator("--", "post--")
    object `pre++` extends UnaryOperator("++", "pre++")
    object `pre--` extends UnaryOperator("--", "pre--")
    object `delete` extends UnaryOperator("delete")
    object `void` extends UnaryOperator("void")
    object `typeof` extends UnaryOperator("typeof")
    object `unary+` extends UnaryOperator("+", "unary+")
    object `unary-` extends UnaryOperator("-", "unary-")
    object `~` extends UnaryOperator("~")
    object `!` extends UnaryOperator("!")


    // Binary Operators
    object `,` extends BinaryOperator(",")
    object `=` extends BinaryOperator("=")
    object `*=` extends BinaryOperator("*=")
    object `/=` extends BinaryOperator("/=")
    object `%=` extends BinaryOperator("%=")
    object `+=` extends BinaryOperator("+=")
    object `-=` extends BinaryOperator("-=")
    object `<<=` extends BinaryOperator("<<=")
    object `>>=` extends BinaryOperator(">>=")
    object `>>>=` extends BinaryOperator(">>>=")
    object `&=` extends BinaryOperator("&=")
    object `^=` extends BinaryOperator("^=")
    object `|=` extends BinaryOperator("|=")
    object `**=` extends BinaryOperator("**=")
    object `&&` extends BinaryOperator("&&")
    object `||` extends BinaryOperator("||")
    object `&` extends BinaryOperator("&")
    object `^` extends BinaryOperator("^")
    object `|` extends BinaryOperator("|")
    object `!==` extends BinaryOperator("!==")
    object `===` extends BinaryOperator("===")
    object `!=` extends BinaryOperator("!=")
    object `==` extends BinaryOperator("==")
    object `<=` extends BinaryOperator("<=")
    object `>=` extends BinaryOperator(">=")
    object `<` extends BinaryOperator("<")
    object `>` extends BinaryOperator(">")
    object `in` extends BinaryOperator("in")
    object `instanceof` extends BinaryOperator("instanceof")
    object `>>>` extends BinaryOperator(">>>")
    object `>>` extends BinaryOperator(">>")
    object `<<` extends BinaryOperator("<<")
    object `binary+` extends BinaryOperator("+", "binary+")
    object `binary-` extends BinaryOperator("-", "binary-")
    object `*` extends BinaryOperator("*")
    object `/` extends BinaryOperator("/")
    object `%` extends BinaryOperator("%")
    object `**` extends BinaryOperator("**")
  }

  sealed abstract class AstNode

  sealed case class Block(statement: Seq[Statement]) extends AstNode

  sealed abstract class Statement extends AstNode

  sealed abstract class NonDeclarationStatement extends Statement

  sealed case class EmptyStatement() extends Statement

  sealed case class BlockStatement(block: Block) extends Statement

  sealed case class VariableStatement(declType: VariableDeclarationType.VariableDeclarationType, decls: Seq[Ast.VariableDeclaration]) extends Statement

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

  sealed case class ClassDeclaration(clazz: Class) extends Declaration

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

  sealed case class BinaryOperatorExpression(operator: BinaryOperator, left: Expression, right: Expression) extends Expression
  sealed case class ConditionalOperatorExpression(condition: Expression, success: Expression, fail: Expression) extends Expression
  sealed case class UnaryExpression(operator: UnaryOperator, expression: Expression) extends Expression

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

  sealed case class Parameters(parameters: Seq[BindingElement], rest: Option[Binding])
  sealed case class Function(closureType: ClosureType.ClosureType, identifier: Option[String], parameters: Parameters, body: Seq[Statement]) extends PrimaryExpression
  sealed case class Class(identifier: Option[String], heritage: Option[Expression], methods: Seq[ClassMember]) extends PrimaryExpression

  sealed case class ClassMember(method: MethodDefinition, isStatic: Boolean) extends AstNode
  sealed abstract class MethodDefinition extends AstNode
  sealed case class Method(closureType: ClosureType.ClosureType, name: PropertyName, parameters: Parameters, body: Seq[Statement]) extends MethodDefinition
  sealed case class PropertyGetter(name: PropertyName, body: Seq[Statement]) extends MethodDefinition
  sealed case class PropertySetter(name: PropertyName, parameter: BindingElement,  body: Seq[Statement]) extends MethodDefinition

  sealed case class YieldExpression(expression: Option[Expression], array: Boolean) extends Expression

  sealed case class ExpressionStatement(expression: Expression) extends Statement

  sealed case class AwaitExpression(expression: Expression) extends Expression

}