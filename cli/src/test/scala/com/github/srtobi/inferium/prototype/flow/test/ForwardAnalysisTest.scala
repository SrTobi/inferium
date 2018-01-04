package com.github.srtobi.inferium.prototype.flow.test

import com.github.srtobi.inferium.prototype.LangParser
import com.github.srtobi.inferium.prototype.flow.ForwardFlowAnalysis.IniObject
import com.github.srtobi.inferium.prototype.flow._
import fastparse.core.Parsed
import org.scalatest.{FlatSpec, Inside, Matchers}

class ForwardAnalysisTest extends FlatSpec with Inside with Matchers{

    private def analyse(code: String): (Option[HeapMemory], Value) = {
        inside (LangParser.script.parse(code)) {
            case Parsed.Success(script, _) =>

                val global = IniObject("rand" -> BoolValue)

                val analysis = ForwardFlowAnalysis.create(script, Solver, new IterationHeap, global)
                analysis.analyse()
                return (analysis.lastHeap, analysis.scriptReturn.asValue)
        }
    }

    "ForwardAnalysis" should "handle basic return types" in {
        analyse(
            """
              |return true
            """.stripMargin) should matchPattern { case (Some(_), SpecificBoolValue(true)) => }

        analyse(
            """
              |return
            """.stripMargin) should matchPattern { case (Some(_), UndefinedValue) => }


        analyse(
            """
              |return 10 - 5
            """.stripMargin) should matchPattern { case (Some(_), SpecificNumberValue(5)) => }

        analyse(
            """
              |return (1).xxx
            """.stripMargin) should matchPattern { case (Some(_), UndefinedValue) => }
    }

    it should "unify return values" in {
        analyse(
            """
              |if (rand) {
              |  return undefined
              |} else {
              |  return 5
              |}
            """.stripMargin) should matchPattern { case (Some(_), UnionValue(Seq(UndefinedValue, SpecificNumberValue(5)))) => }

        analyse(
            """
              |if (rand) {
              |  return 6
              |} else {
              |  return 5
              |}
            """.stripMargin) should matchPattern { case (Some(_), NumberValue) => }
    }


    it should "unify object properties" in {
        inside(analyse(
            """
              |var x = 0
              |if (rand) {
              |  x = "a"
              |} else {
              |  x = "b"
              |}
              |return x
            """.stripMargin)) { case (Some(_), res) => UnionSet("a", "b") shouldBe res}

        inside(analyse(
            """
              |var x = "b"
              |if (rand) {
              |  x = "a"
              |}
              |return x
            """.stripMargin)) { case (Some(_), res) => UnionSet("a", "b") shouldBe res}


        inside(analyse(
            """
              |var x = "b"
              |if (rand) {
              |  x = "a"
              |} else {
              |
              |}
              |return x
            """.stripMargin)) { case (Some(_), res) => UnionSet("a", "b") shouldBe res}
    }

    it should "unify objects and keep correct track of properties" in {

        inside(analyse(
            """
              |var a = { prop: "a" }
              |var b = { prop: "b" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet("a", "b") shouldBe res}


        inside(analyse(
            """
              |var a = { prop: "a" }
              |var b = { prop: "b" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |x.prop = "c"
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => Value("c") shouldBe res}

        inside(analyse(
            """
              |var a = { prop: "a" }
              |var b = { prop: "b" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |x.prop = "c"
              |a.prop = "d"
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet("c", "d") shouldBe res}

        inside(analyse(
            """
              |var a = { prop: "a" }
              |var b = { prop: "b" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |x.prop = "c"
              |return a.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet("a", "c") shouldBe res}


        inside(analyse(
            """
              |var a = { prop: "a" }
              |var b = { prop: "b" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |x.prop = "c"
              |return b.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet("b", "c") shouldBe res}

        inside(analyse(
            """
              |var a = { prop: "a" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = 1
              |}
              |x.prop = "c"
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet("c", UndefinedValue) shouldBe res}

        inside(analyse(
            """
              |var a = { prop: "a" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = 1
              |}
              |x.prop = "c"
              |a.prop = "d"
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet("d", UndefinedValue) shouldBe res}
    }

    it should "filter Values which can not be property accessed" in {

        inside(analyse(
            """
              |var x = { prop: "a" }
              |if (rand) {
              |  x = undefined
              |}
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => Value("a") shouldBe res}
    }
}
