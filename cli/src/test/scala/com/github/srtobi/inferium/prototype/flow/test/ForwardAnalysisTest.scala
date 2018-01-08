package com.github.srtobi.inferium.prototype.flow.test

import com.github.srtobi.inferium.prototype.LangParser
import com.github.srtobi.inferium.prototype.flow.Heap.{IniEntity, IniObject}
import com.github.srtobi.inferium.prototype.flow._
import fastparse.core.Parsed
import org.scalatest.{FlatSpec, Inside, Matchers}

class ForwardAnalysisTest extends FlatSpec with Inside with Matchers{
    import Heap.{IniEntity => E}

    private def analyse(code: String): (HeapMemory, IniEntity) = {
        inside (LangParser.script.parse(code)) {
            case Parsed.Success(script, _) =>

                val global = IniObject("rand" -> BoolValue)

                val analysis = ForwardFlowAnalysis.create(script, Solver, new IterationHeap, global)
                analysis.analyse()
                val heap = analysis.globalHeap
                val scriptValue = analysis.scriptReturn
                val (_, result) = heap.toIniEntity(Seq(scriptValue)).head

                return (heap, result)
        }
    }

    "ForwardAnalysis" should "handle basic return types" in {
        inside(analyse(
            """
              |return true
            """.stripMargin)) { case (_, res) => res shouldBe E(TrueValue)}

        inside(analyse(
            """
              |return
            """.stripMargin)) { case (_, res) => res shouldBe E(UndefinedValue)}


            inside(analyse(
            """
              |return 10 - 5
            """.stripMargin)) { case (_, res) => res shouldBe E(5)}

            inside(analyse(
            """
              |return (1).xxx
            """.stripMargin)) { case (_, res) => res shouldBe E(UndefinedValue)}
    }

    it should "unify return values" in {
        inside(analyse(
            """
              |if (rand) {
              |  return undefined
              |} else {
              |  return 5
              |}
            """.stripMargin)) { case (_, res) => res shouldBe E(UndefinedValue, 5) }

        inside(analyse(
            """
              |if (rand) {
              |  return 6
              |} else {
              |  return 5
              |}
            """.stripMargin)) { case (_, res) => res shouldBe E(NumberValue) }
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
            """.stripMargin)) { case (_, res) => res shouldBe E("a", "b")}

        inside(analyse(
            """
              |var x = "b"
              |if (rand) {
              |  x = "a"
              |}
              |return x
            """.stripMargin)) { case (_, res) => res shouldBe E("a", "b")}


        inside(analyse(
            """
              |var x = "b"
              |if (rand) {
              |  x = "a"
              |} else {
              |  x = "c"
              |}
              |return x
            """.stripMargin)) { case (_, res) => res shouldBe E("a", "c")}
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
            """.stripMargin)) { case (_, res) => res shouldBe E("a", "b")}


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
            """.stripMargin)) { case (_, res) => res shouldBe E("c")}

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
            """.stripMargin)) { case (_, res) => res shouldBe E("c", "d")}

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
            """.stripMargin)) { case (_, res) => res shouldBe E("a", "c")}


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
            """.stripMargin)) { case (_, res) => res shouldBe E("b", "c")}

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
            """.stripMargin)) { case (_, res) => res shouldBe E("a", "b", "c")}

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
            """.stripMargin)) { case (_, res) => res shouldBe E("c", UndefinedValue)}


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
            """.stripMargin)) { case (_, res) => res shouldBe E("d", UndefinedValue)}
    }

    it should "filter Values which can not be property accessed" in {

        inside(analyse(
            """
              |var x = { prop: "a" }
              |if (rand) {
              |  x = undefined
              |}
              |return x.prop
            """.stripMargin)) { case (_, res) => res shouldBe E("a")}

        inside(analyse(
            """
              |var x = { prop: "a" }
              |if (rand) {
              |  x = 1
              |}
              |return x.prop
            """.stripMargin)) { case (_, res) => res shouldBe E(UndefinedValue, "a")}

        inside(analyse(
            """
              |var x = { prop: "a" }
              |var y = x
              |if (rand) {
              |  y = undefined
              |}
              |return y.prop
            """.stripMargin)) { case (_, res) => res shouldBe E("a")}

        inside(analyse(
            """
              |var x = { prop: "a" }
              |var y = x
              |if (rand) {
              |  y = undefined
              |}
              |return x.prop
            """.stripMargin)) { case (_, res) => res shouldBe E("a")}
    }

    it should "handle calls to pure functions correctly" in {

        inside(analyse(
            """
              |var f = () => {
              |  return 8
              |}
              |return f()
            """.stripMargin)) { case (_, res) => res shouldBe E(8)}

        inside(analyse(
            """
              |var f = (a) => {
              |  return a
              |}
              |return f(9)
            """.stripMargin)) { case (_, res) => res shouldBe E(9)}


        inside(analyse(
            """
              |var f = (a, b) => {
              |  return a - b
              |}
              |return f(11, 10)
            """.stripMargin)) { case (_, res) => res shouldBe E(1)}


        inside(analyse(
            """
              |var f = (a, b) => {
              |  return a - b
              |}
              |return f(11, 10)
            """.stripMargin)) { case (_, res) => res shouldBe E(1)}
    }

    it should "handle unmatching parameter numbers correctly" in {

        inside(analyse(
            """
              |var f = (a, b) => {
              |  return b
              |}
              |return f(11)
            """.stripMargin)) { case (_, res) => res shouldBe E(UndefinedValue)}


        inside(analyse(
            """
              |var f = (a, b) => {
              |  return a - b
              |}
              |return f(11, 10, "blub")
            """.stripMargin)) { case (_, res) => res shouldBe E(1)}
    }

    it should "handle accesses to other scopes correctly" in {

        inside(analyse(
            """
              |var x = 0
              |var f = () => {
              |  return x
              |}
              |return f()
            """.stripMargin)) { case (_, res) => res shouldBe E(0)}


        inside(analyse(
            """
              |var x = 0
              |var f = () => {
              |  return x
              |}
              |x = 5
              |return f()
            """.stripMargin)) { case (_, res) => res shouldBe E(5)}


        inside(analyse(
            """
              |var x = 0
              |var f = (a) => {
              |  return x - a
              |}
              |x = 9
              |return f(x)
            """.stripMargin)) { case (_, res) => res shouldBe E(0)}


        inside(analyse(
            """
              |var x = 0
              |var f = () => {
              |  x = 5
              |  return 12
              |}
              |return f() - x
            """.stripMargin)) { case (_, res) => res shouldBe E(7)}
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
            """.stripMargin)) { case (_, res) => res shouldBe E("a", "b")}
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
            """.stripMargin)) { case (_, res) => res shouldBe E("a")}

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
            """.stripMargin)) { case (_, res) => res shouldBe E("a", "b")}
    }

    it should "not call uncallable values" in {

        inside(analyse(
            """
              |var a = 1
              |return a()
            """.stripMargin)) { case (_, res) => res shouldBe E(NeverValue)}
    }

    it should "filter uncallable values" in {
        inside(analyse(
            """
              |if (rand) {
              |  var f = () => {}
              |} else {
              |  f = "test"
              |}
              |f()
              |return f
            """.stripMargin)) { case (_, res) => res shouldBe IniObject()}
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
            """.stripMargin)) { case (_, res) => res shouldBe E(1)}

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
            """.stripMargin)) { case (_, res) => res shouldBe E(1)}

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
            """.stripMargin)) { case (_, res) => res shouldBe E("inbetween")}

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
            """.stripMargin)) { case (_, res) => res shouldBe E(50)}


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
            """.stripMargin)) { case (_, res) => res shouldBe E(50, "correct")}


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
            """.stripMargin)) { case (_, res) => res shouldBe E(true, "bla")}

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
            """.stripMargin)) { case (_, res) => res shouldBe E(false, "bla")}

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
            """.stripMargin)) { case (_, res) => res shouldBe E(50, "correct")}
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
            """.stripMargin)) { case (_, res) => res shouldBe E(true)}


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
            """.stripMargin)) { case (_, res) => res shouldBe E(false)}


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
            """.stripMargin)) { case (_, res) => res shouldBe IniObject("cond" -> true, "prop" -> "true") }

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
            """.stripMargin)) { case (_, res) => res shouldBe E("true")}

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
              |  x.prop = "haha"
              |}
              |return x.prop
            """.stripMargin)) { case (_, res) => res shouldBe E("false", "haha")}


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
              |} else {
              |  x.prop = "haha"
              |}
              |return x.prop
            """.stripMargin)) { case (_, res) => res shouldBe E("true", "haha")}


        inside(analyse(
            """
              |var a = { cond: true, prop: "true" }
              |var b = { cond: false, prop: "false" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |var cond = x.cond
              |if (cond) {
              |  x.prop = "haha"
              |}
              |return x.prop
            """.stripMargin)) { case (_, res) => res shouldBe E("false", "haha")}


        inside(analyse(
            """
              |var a = { cond: true, prop: "true" }
              |var b = { cond: false, prop: "false" }
              |if (rand) {
              |  var x = a
              |} else {
              |  x = b
              |}
              |if (x.cond) {
              |  b.prop = "haha"
              |}
              |return x.prop
            """.stripMargin)) { case (_, res) => res shouldBe E("true", "false", "haha")}
    }
}
