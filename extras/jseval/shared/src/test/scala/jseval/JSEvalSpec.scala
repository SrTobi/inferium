package jseval

import org.scalatest.{FlatSpec, Matchers}
import upickle.Js

class JSEvalSpec extends FlatSpec with Matchers {

    val jseval = new JSEval

    "JSEval" should "return basic values" in {
        jseval.eval("true") shouldBe Js.True
        jseval.eval("false") shouldBe Js.False


        jseval.eval("1") shouldBe Js.Num(1)
        jseval.eval("\"blub\"") shouldBe Js.Str("blub")
    }

    it should "return objects" in {
        jseval.eval("({})") shouldBe Js.Obj()
        jseval.eval("({ test: 3 })") shouldBe Js.Obj(("test", Js.Num(2)))
    }
}
