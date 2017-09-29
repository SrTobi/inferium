package com.github.srtobi.inferium.cli.jsparser

import fastparse.noApi._
import WsApi._

import scala.io.Source

object Expression {
  val var_decl = P(lexical.variable ~ "=" ~ (((lexical.string | lexical.identifier) | lexical.number) ~ Maths.expr.rep.?) ~ ";" ~ End).!
  val var_decl_f = P(lexical.variable ~ "=" ~ func_call)
  val var_dec_arr = P(lexical.variable ~ "=" ~ lexical.sarray ~ ";")
  val calc: Parser[Any] = P(lexical.number.! ~ (lexical.arithOperator.! ~ lexical.number.!).rep.? | "")
  val vasign = P(lexical.identifier ~ lexical.asignOperator ~ Maths.expr | lexical.string | lexical.identifier ~ ";")

  //def foo(i: Int): String = "The number is " + i


  val binary = P(("0" | "1").rep.!)
  val binaryNum = P(binary.map(Integer.parseInt(_, 2)))
  val output = {
    val alert = P("window.alert(" ~ Maths.expr ~ ");")
    val write = P("document.write(" ~ Maths.expr ~ ");")
    val innerHtml = P("document.getElementById(" ~ lexical.string ~ ").innerHTML = " ~ Maths.expr ~ ";")
    val log = P("console.log(" ~ Maths.expr ~ ");")
    P(alert | log | write | innerHtml)
  }
  val arr_conv = {
    val str = P(lexical.identifier | Maths.expr ~ ".toString();")
    val join = P(lexical.identifier ~ ".toJoin(\"" ~ AnyChar ~ "\");")
    val pop = P(lexical.identifier ~ ".pop();")
    val push = P(lexical.identifier ~ ",push();")
    val shift = P(lexical.identifier ~ ".shift();")
    val unshift = P(lexical.identifier ~ ".unshift();")
    val index = P(lexical.identifier ~ "[" ~ Maths.expr ~ "];")
    val del = P("delete" ~ index)
    val splice = P(lexical.identifier ~ ".splice(" ~ Maths.factor ~ "," ~ Maths.factor ~ lexical.string.? ~ "," ~ lexical.string.?)
    val sort = P(lexical.identifier ~ "sort();")
    val reverse = P(lexical.identifier ~ "reverse();")
    P(str | join | pop | push)
  }
  //val type = P("typeof" ~ )
  val statement = P((var_decl|error|num|math|str|loop| func_decl| var_decl_f | var_dec_arr | vasign | output | ret  ).rep )

  val leftTag = P("<" ~ (!">" ~ AnyChar).rep(1).! ~ ">")
  val int = P(CharIn('0' to '9')).!.map(_.toInt)
  val expr = P(int ~ "+" ~ int)
  val len = P(lexical.letter).rep
  val func_decl:fastparse.core.Parser[String, Char, String] = P("function" ~ lexical.identifier ~ "(" ~ lexical.identifier~("," ~ lexical.identifier).rep.? ~ ")" ~"{" ~ statement ~ "}").!
  val ret = P("return" ~ lexical.identifier | Maths.expr |lexical.exp~ ";")
  val func_call = P(lexical.identifier ~ "(" ~ (lexical.identifier ~ ",").rep.? ~ ");")

  val str = {
    val new_string = P("new String(" ~ lexical.string ~ ");")
    val length = P(lexical.identifier ~ ".length;")
    val indexOf = P(lexical.identifier ~ ".indexOf(" ~ lexical.string ~ ");")
    val search = P(lexical.identifier ~ ".search(" ~ lexical.string ~ ");")
    var slice = P(lexical.identifier ~ ".slice(" ~ lexical.number ~ ",".? ~ lexical.number.? ~ ");")
    var substr = P(lexical.identifier ~ ".slice(" ~ lexical.number ~ "," ~ lexical.number ~ ");")
    var replace = P(lexical.identifier ~ ".replace(" ~ lexical.string ~ "," ~ lexical.string ~ ");")
    var uc = P(lexical.identifier ~ ".toUpperCase();")
    var lc = P(lexical.identifier ~ ".toLowerCase();")
    var concat = P(lexical.identifier ~ ".concat(" ~ "," ~ lexical.string ~ ");")
    val charat = P(lexical.identifier ~ ".charAt(" ~ Maths.factor ~ ");")
    val charCodeat = P(lexical.identifier ~ ".charCodeAt(" ~ Maths.factor ~ ");")
    P("var"~lexical.identifier~"="~(new_string | length | indexOf | search | slice | substr | replace | uc | lc | concat | charat | charCodeat))
  }

  val num = {
    val exp = P(lexical.identifier ~ ".toExponential(" ~ Maths.expr ~ ");")
    val fix = P(lexical.identifier ~ ".toFixed(" ~ Maths.expr ~ ");")
    val precision = P(lexical.identifier ~ ".toPrecision(" ~ Maths.expr ~ ");")
    val valof = P(lexical.identifier | Maths.expr ~ ".valueOf();")
    val numb = P("number(" ~ lexical.identifier ~ ");")
    val parseint = P("parseInt(" ~ lexical.number ~ ");")
    val parsefloat = P("parseFloat(" ~ lexical.number ~ ");")
    P(lexical.identifier~"="~ (exp | fix | valof | numb | parseint | parsefloat | precision))
  }

  val math = {
    val min = P("Math.min(" ~/ Maths.min ~ ");")
    val max = P("Math.max(" ~/ Maths.max ~ ");")
    val random = P("Math.random();")
    val round = P("Math.round(" ~ Maths.factor ~ ");")
    val ceil = P("Math.ceil(" ~ Maths.factor ~ ");")
    val floor = P("Math.floor(" ~ Maths.factor ~ ");")
    P(lexical.identifier~"="~ (max | min | random | round | ceil | floor))
  }

  val condition:fastparse.core.Parser[String, Char, String] = {
    val If = P("if" ~ "(" ~ lexical.identifier ~ lexical.compOperator ~ lexical.number ~ ")" ~ "{" ~ statement ~ "}")
    val Else = P(("{" ~ statement ~ "}") | "")
    val Case = P("case" ~ lexical.number | lexical.string | lexical.letter ~ ":" ~ statement)
    val default = P("default:" ~ statement)
    val switch = P("switch(" ~ lexical.identifier ~ ")" ~ "{" ~ Case.rep ~ default ~ "}")
    P(If | Else | switch).!

  }

  val loop :fastparse.core.Parser[String, Char, String]= {
    var asignStat = P(lexical.identifier ~ "=" ~ Maths.factor ~ ";")
    var compStat = P(lexical.identifier ~ lexical.compOperator ~ (lexical.identifier | lexical.string | lexical.number))
    var increStat = P((lexical.identifier ~ lexical.incredecre) | (lexical.identifier ~ lexical.asignOperator ~ Maths.expr) ~ ";")
    val forL = P("for" ~ "(" ~ asignStat ~ compStat ~ increStat ~ ")" ~ "{" ~ statement ~ "}")
    var whileL = P("while" ~ "(" ~ compStat ~ ")" ~ "{" ~ statement ~ "}")
    val doWhile = P("do" ~ "{" ~ statement ~ "}" ~ "while" ~ "(" ~ compStat ~ ")" ~ ";")
    P(forL | whileL | doWhile).!
  }

  val error:fastparse.core.Parser[String, Char, String] = {
    val trY = P("try" ~ "{" ~ statement ~ "}")
    val catcH = P("catch" ~ "(" ~ lexical.identifier ~ ")" ~ "{" ~ statement ~ "}")
    val throW = P("throw" ~ Maths.expr | lexical.string)
    val finallY = P("finally" ~ "{" ~ statement ~ "}")
    P(trY | catcH | throW | finallY).!
  }


  def main(arg: Array[String]): Unit = {
    println("yeyyyyy")
    val filename = "fileopen.js"
    val fileContents = "var test = 2;"
    //println(fileContents)
    val res = statement.parse(fileContents)
    println(res)
    //println(.parse())

  }
}