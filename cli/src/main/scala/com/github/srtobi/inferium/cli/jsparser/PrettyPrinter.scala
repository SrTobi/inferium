package com.github.srtobi.inferium.cli.jsparser


private sealed abstract class Piece
private case class Code(text: String) extends Piece
private case class Indented(member: Seq[Piece], n: Int) extends Piece {
  def this(mem: Piece*) = this(mem, 1)
}


private class PrettyPrinter private(){

  import Ast._

  def print(node: Ast.AstNode): Seq[Piece] = node match {
    case Script(statements) => statements.map(print).flatMap(_ :+ Code("\n"))
    case ArrayLiteral(es) => c("[") +: betweenAll(es, c(", ")) :+ c("]")
    case ArrayElement(e) => print(e)
    case ArrayElisionElement() => Seq(c(""))
    case AwaitExpression(expr) => c("await ") +: print(expr) :+ c(";")
    case BinaryOperatorExpression(op, left, right) =>
      print(left) ++ c1(s" ${op.text} ") ++ print(right)
    case Block(stats) => block(stats)
    case BlockStatement(blk) => print(blk)
    case BooleanLiteral(b) => c1(if (b) "true" else "false")
    case BreakStatement(label) => c1("break $label;")
    case Class(id, heritage, member) =>
      (c("class ") +: Seq(
        id.toSeq.flatMap(id => c1(id + " ")),
        heritage.toSeq.flatMap(e => c("extends ") +: print(e))
      ).flatten) ++ block(member)
    case ClassDeclaration(cl) => print(cl)
    case ConditionalOperatorExpression(cond, succ, fail) =>
      Seq(c1("("), print(cond), c1("?"), print(succ), c1(":"), print(fail), c1(")")).flatten
    case ContinueStatement(label) => c1(s"continue $label")
    case DeclarationStatement(decl) => print(decl)
    case EmptyStatement() => c1(";")
    case ExpressionStatement(expr) => print(expr) :+ c(";")
    case Function(ty, id, params, body) =>
      val name = optToName(id)
      Seq(
        Seq(c(ty match {
          case ClosureType.Normal => s"function $name"
          case ClosureType.Async => s"async function $name"
          case ClosureType.Generator => s"function* $name"
          case ClosureType.Arrow => ""
          case ClosureType.AsyncArrow => "async"
        })),
        print(params),
        ty match {
          case ClosureType.Arrow | ClosureType.AsyncArrow => c1(" =>")
          case _ => Seq()
        },
        c1(" "),
        block(body)
      ).flatten
    case FunctionCallExpression(target, args) =>
      print(target) ++ print(args)
    case FunctionDeclaration(decl) => print(decl)
    case IdentifierDeclaration(id, ini) =>
      c(id.identifier) +: ini.map(print(_)).map(c(" = ") +: _).getOrElse(Seq())
    case IdentifierReferenceExpression(id) => c1(id)
    case IdentifierBinding(id) => c1(id)
    case IfStatement(cond, succ, fail) =>
      Seq(
        c1("if ("),
        print(cond),
        c1(")\n"),
        in1(print(succ))
      ).flatten ++
      fail.map(blk => Seq(c("\nelse\n"), in(print(blk)))).getOrElse(Seq())
    case LabelledStatement(label, statements) =>
      in(c1(label + ":"), -1) +: (c1("\n") ++ print(statements))
    case NullLiteral() => c1("null")
    case NumericLiteral(n) => c1(n)
    case Parameters(params, rest) =>
      c("(") +: (betweenAll(params, c(", ")) ++ rest.toSeq.flatMap(r => c(if (params.isEmpty) "..." else ", ...") +: print(r))) :+ c(")")
    case ReturnStatement(expr) => expr match {
      case Some(e) => c("return ") +: print(e) :+ c(";")
      case None => c1("return;")
    }
    case SingleNameBinding(name, default) =>
      c(name) +: default.map(print(_)).map(c(" = ") +: _).getOrElse(Seq())
    case ThisExpression() => c1("this")
    case UnaryExpression(op, expr) =>
      if (op.name.contains("post"))
        c(op.text) +: print(expr)
      else
        print(expr) :+ c(op.text)
    case VariableStatement(ty, decls) =>
      c(ty.toString.toLowerCase + " ") +: betweenAll(decls, c(", ")) :+ c(";")
    case YieldExpression(expr, array) =>
      val yieldStr = if(array) "yield*" else "yield"
      expr match {
        case Some(e) => c(yieldStr + " ") +: print(e) :+ c(";")
        case None => c1(yieldStr + ";")
      }
  }

  private def optToName(id: Option[String]) = id.map(_ + " ").getOrElse("")
  private def block(nodes: Seq[AstNode]): Seq[Piece] = Seq(c("{\n"), in(nodes.flatMap(print(_) :+ c("\n"))), c("}"))
  private def betweenAll(nodes: Seq[AstNode], sep: Piece): Seq[Piece] = nodes.flatMap(sep +: print(_)) match {
    case _ +: tail => tail
    case _ => Seq()
  }

  //private def c(str: String) = Code(str)
  private def nl(str: String) = Code(str + "\n")
  private def c(str: String) = Code(str)
  private def c1(str: String) = Seq(Code(str))
  private def in(mem: Seq[Piece], ind: Int = 1) = Indented(mem, ind)
  private def in(mem: Piece*): Indented = in(mem)
  private def in1(mem: Seq[Piece]) = Seq(Indented(mem, 1))
}

object PrettyPrinter {

  private def stringify(piece: Piece, target: StringBuilder, indent: Int, indentSeq: String): Unit = piece match {
    case Code(code) => target ++= code.replace("\n", "\n" + Seq.fill(Math.max(indent, 0))(indentSeq).mkString)
    case Indented(member :+ last, n) =>
      target ++= indentSeq
      member.foreach(stringify(_, target, indent + n, indentSeq))
      stringify(last, target, indent, indentSeq)
    case _ =>
  }

  def print(ast: Ast.AstNode, indent: Boolean = true, startIndent: Int = 0): String = {
    val sb = new StringBuilder
    val pp = new PrettyPrinter()
    val seq = pp.print(ast)
    seq.foreach(stringify(_, sb, 0, "  "))
    return sb.toString
  }
}