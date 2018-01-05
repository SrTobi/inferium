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
              |var b = { prop: "b" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |var y = x
              |if (rand) {
              |  y.prop = "c"
              |}
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet("a", "b", "c") shouldBe res}

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

    it should "handle unmatching parameter numbers correctly" in {

        inside(analyse(
            """
              |var f = (a, b) => {
              |  return b
              |}
              |return f(11)
            """.stripMargin)) { case (Some(_), res) => UndefinedValue shouldBe res}


        inside(analyse(
            """
              |var f = (a, b) => {
              |  return a - b
              |}
              |return f(11, 10, "blub")
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

    it should "handle context correctly" in {

        inside(analyse(
            """
              |var f = () => {
              |  var x = undefined
              |  return (a) => {
              |    var old = x
              |    x = a
              |    return old
              |  }
              |}
              |var a = f()
              |var b = f()
              |a("a")
              |b("b")
              |return a("c")
            """.stripMargin)) { case (Some(_), res) => Value("a") shouldBe res}

        inside(analyse(
            """
              |var f = () => {
              |  var x = undefined
              |  return (a) => {
              |    var old = x
              |    x = a
              |    return old
              |  }
              |}
              |var a = f()
              |a("a")
              |if (rand) {
              |  a("b")
              |}
              |return a("c")
            """.stripMargin)) { case (Some(_), res) => UnionSet("a", "b") shouldBe res}
    }

    it should "not call uncallable values" in {

        inside(analyse(
            """
              |var a = 1
              |return a()
            """.stripMargin)) { case (None, res) => NeverValue shouldBe res}
    }

    it should "filter uncabbable values" in {
        analyse(
            """
              |if (rand) {
              |  var f = () => {}
              |} else {
              |  f = "test"
              |}
              |f()
              |return f
            """.stripMargin) should matchPattern { case (Some(_), _: FunctionValue) =>}
    }

    it should "filter depended references" in {

        inside(analyse(
            """
              |if (rand) {
              |  var a = 1
              |} else {
              |  a = undefined
              |}
              |var b = a
              |b.prop
              |return b
            """.stripMargin)) { case (Some(_), res) => Value(1) shouldBe res}

        inside(analyse(
            """
              |if (rand) {
              |  var a = 1
              |} else {
              |  a = undefined
              |}
              |var b = a
              |b.prop
              |return a
            """.stripMargin)) { case (Some(_), res) => Value(1) shouldBe res}

        inside(analyse(
            """
              |if (rand) {
              |  var a = 1
              |} else {
              |  a = undefined
              |}
              |var b = a
              |a = "inbetween"
              |b.prop
              |return a
            """.stripMargin)) { case (Some(_), res) => Value("inbetween") shouldBe res}

    }

    it should "filter variables in if-conditions" in {

        inside(analyse(
            """
              |if (rand) {
              |  var a = 50
              |} else {
              |  a = undefined
              |}
              |if (a) {
              | return a
              |}
              |undefined.prop
            """.stripMargin)) { case (Some(_), res) => Value(50) shouldBe res}


        inside(analyse(
            """
              |if (rand) {
              |  var a = 50
              |} else {
              |  a = undefined
              |}
              |if (a) {
              | return a
              |} else {
              | if (a) {
              |   return "incorrect"
              | } else {
              |   return "correct"
              | }
              |}
              |undefined.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet(50, "correct") shouldBe res}


        inside(analyse(
            """
              |var o = {}
              |if (rand) {
              |  o.a = true
              |} else {
              |  o.a = false
              |}
              |var b = o.a
              |if (b) {
              |  return o.a
              |} else {
              |  return "bla"
              |}
            """.stripMargin)) { case (Some(_), res) => UnionSet(true, "bla") shouldBe res}

        inside(analyse(
            """
              |var o = {}
              |if (rand) {
              |  o.a = true
              |} else {
              |  o.a = false
              |}
              |var b = o.a
              |if (b) {
              |  return "bla"
              |} else {
              |  return o.a
              |}
            """.stripMargin)) { case (Some(_), res) => UnionSet(false, "bla") shouldBe res}

        inside(analyse(
            """
              |if (rand) {
              |  var a = 50
              |} else {
              |  a = undefined
              |}
              |if (a) {
              | return a
              |} else {
              | if (a) {
              |   return "incorrect"
              | } else {
              |   return "correct"
              | }
              |}
              |undefined.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet(50, "correct") shouldBe res}
    }

    it should "filter objects according to their properties" in {

        inside(analyse(
            """
              |var a = { cond: true, prop: "true" }
              |var b = { cond: false, prop: "false" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |
              |if (x.cond) {
              |  return x.cond
              |}
              |undefined.prop
            """.stripMargin)) { case (Some(_), res) => Value(true) shouldBe res}


        inside(analyse(
            """
              |var a = { cond: true, prop: "true" }
              |var b = { cond: false, prop: "false" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |
              |if (x.cond) {
              |  undefined.prop
              |} else {
              |  return x.cond
              |}
            """.stripMargin)) { case (Some(_), res) => Value(false) shouldBe res}


        inside(analyse(
            """
              |var a = { cond: true, prop: "true" }
              |var b = { cond: false, prop: "false" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |
              |if (x.cond) {
              |  return x
              |}
              |undefined.prop
            """.stripMargin)) { case (Some(_), res) => assert(res.isInstanceOf[ObjectValue]); assert(!res.isInstanceOf[UnionValue])}

        inside(analyse(
            """
              |var a = { cond: true, prop: "true" }
              |var b = { cond: false, prop: "false" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |
              |if (x.cond) {
              |  return x.prop
              |}
              |undefined.prop
            """.stripMargin)) { case (Some(_), res) => Value("true") shouldBe res}

        /*inside(analyse(
            """
              |var a = { cond: true, prop: "true" }
              |var b = { cond: false, prop: "false" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |
              |if (x.cond) {
              |  x.prop = "haha"
              |}
              |return x.prop
            """.stripMargin)) { case (Some(_), res) => UnionSet("false", "haha") shouldBe res}*/
    }
}
