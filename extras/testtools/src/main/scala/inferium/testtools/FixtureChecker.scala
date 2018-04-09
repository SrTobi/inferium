package inferium.testtools

import jseval.{JSEval, JSEvalException}
import upickle.Js

import scala.util.Try

class FixtureChecker {
    private val js = new JSEval
    def check(source: String): Try[Boolean] = {

        val template = Resources.files("js-test-env.js")
        val code = template.replace("// <$test-case$>", source)
        Try(js.eval(code) == Js.Str("ok"))
    }
}
