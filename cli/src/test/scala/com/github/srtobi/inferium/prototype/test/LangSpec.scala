package com.github.srtobi.inferium.prototype.test

import com.github.srtobi.inferium.prototype.LangParser
import fastparse.core.Parsed
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}

class LangSpec extends FlatSpec with TableDrivenPropertyChecks with Matchers {

    val correctPrograms = Table(
        ("code", "equivalent"),
        ("""
          |var x = 0
        """.stripMargin,
            None),
        ("""
          |var y = 99
          |var x = () => {
          | return 0
          |}
        """.stripMargin,
            None),
        ("""
          |var y = 0
          |var x = () => {
          | return y
          |}
          |x(2, 3 + 3)
        """.stripMargin,
            None),
        ("""
          |var j = x; if (test) {
          | j = { xxx: "test"}
          |}
        """.stripMargin,
            None),
        ("""
          |x()()
        """.stripMargin,
            None),
        ("""
          |y().x()()
        """.stripMargin,
            None),
        ("""
          |3; 3
        """.stripMargin,
            None),
        ("""
          |test;  if(xxx) {}
        """.stripMargin,
            None),
        ("""
          |test  ;  if(xxx) {}
        """.stripMargin,
            None),
        ("""
          |test  ;if(xxx) {}
        """.stripMargin,
            None),
        ("""
          |var x = () => test
          |v()
        """.stripMargin,
            None),
        ("""
          |var x = () => 3 + " test"
          |() => 3; if (true) () => false
        """.stripMargin,
            None),
        ("""
          |if (true)
          |  if (false)
          |    true
          |  else
          |    if ((() => true)())
          |      test()
        """.stripMargin,
            Some(
                """
                  |if (true) {
                  |  if (false) {
                  |    true
                  |  } else {
                  |    if((() => {return true})()) {
                  |      test()
                  |    }
                  |  }
                  |}
                """.stripMargin
            )),
        ("""
          |;;;;
        """.stripMargin,
            Some("")),
        ("""
           |if (test) true else false
         """.stripMargin,
            Some("if (test) {true} else {false}")),
    )

    "LangParser" should "parse correct programs" in {
        forAll(correctPrograms) {
            (prog, alt) =>
                LangParser.script.parse(prog) should matchPattern { case Parsed.Success(_, _) => }

                whenever(alt.isDefined) {
                    LangParser.script.parse(alt.get) should matchPattern { case Parsed.Success(_, _) => }
                }
        }
    }

    it should "produce equivalent programs" in {
        forAll(correctPrograms) {
            (prog, alt) =>
                whenever(alt.isDefined) {
                    val progAst = LangParser.script.parse(prog).get.value
                    val altAst = LangParser.script.parse(alt.get).get.value
                    progAst shouldBe altAst
                }
        }
    }
}
