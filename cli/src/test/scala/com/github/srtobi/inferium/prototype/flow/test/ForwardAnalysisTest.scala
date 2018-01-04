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

        inside(analyse(
            """
              |var x = { prop: "a" }
              |if (rand) {
              |  x = 1
              |}
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet(UndefinedValue, "a") shouldBe res}

        inside(analyse(
            """
              |var x = { prop: "a" }
              |var y = x
              |if (rand) {
              |  y = undefined
              |}
              |return y.prop
            """.stripMargin)) { case (Some(_), res) => Value("a") shouldBe res}

        inside(analyse(
            """
              |var x = { prop: "a" }
              |var y = x
              |if (rand) {
              |  y = undefined
              |}
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => Value("a") shouldBe res}
    }

    it should "handle calls to pure functions correctly" in {

        inside(analyse(
            """
              |var f = () => {
              |  return 8
              |}
              |return f()
            """.stripMargin)) { case (Some(_), res) => Value(8) shouldBe res}

        inside(analyse(
            """
              |var f = (a) => {
              |  return a
              |}
              |return f(9)
            """.stripMargin)) { case (Some(_), res) => Value(9) shouldBe res}


        inside(analyse(
            """
              |var f = (a, b) => {
              |  return a - b
              |}
              |return f(11, 10)
            """.stripMargin)) { case (Some(_), res) => Value(1) shouldBe res}


        inside(analyse(
            """
              |var f = (a, b) => {
              |  return a - b
              |}
              |return f(11, 10)
            """.stripMargin)) { case (Some(_), res) => Value(1) shouldBe res}
    }

    it should "handle accesses to other scopes correctly" in {

        inside(analyse(
            """
              |var x = 0
              |var f = () => {
              |  return x
              |}
              |return f()
            """.stripMargin)) { case (Some(_), res) => Value(0) shouldBe res}


        inside(analyse(
            """
              |var x = 0
              |var f = () => {
              |  return x
              |}
              |x = 5
              |return f()
            """.stripMargin)) { case (Some(_), res) => Value(5) shouldBe res}


        inside(analyse(
            """
              |var x = 0
              |var f = (a) => {
              |  return x - a
              |}
              |x = 9
              |return f(x)
            """.stripMargin)) { case (Some(_), res) => Value(0) shouldBe res}


        inside(analyse(
            """
              |var x = 0
              |var f = () => {
              |  x = 5
              |  return 12
              |}
              |return f() - x
            """.stripMargin)) { case (Some(_), res) => Value(7) shouldBe res}
    }

    it should "handle merged functions correctly" in {

        inside(analyse(
            """
              |var f = 0
              |if(rand) {
              |  f = () => {
              |    return "a"
              |  }
              |} else {
              |  f = () => {
              |    return "b"
              |  }
              |}
              |return f()
            """.stripMargin)) { case (Some(_), res) => UnionSet("a", "b") shouldBe res}
    }
}
