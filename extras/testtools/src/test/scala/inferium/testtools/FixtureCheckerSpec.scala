package inferium.testtools

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Success, Failure}

class FixtureCheckerSpec extends FlatSpec with Matchers {

    val checker = new FixtureChecker

    "The FixtureChecker" should "return success if script runs without problems" in {
        val code =
            """
              |var a
              |a = 4
              |if (a <= 0) {
              |  throw new Error("Should not happen")
              |}
            """.stripMargin
        checker.check(code) shouldBe Success(true)
    }

    it should "return a failure if the script throws an exception" in {
        val code =
            """
              |throw new Error("this error should happen")
            """.stripMargin
        checker.check(code) should matchPattern { case Failure(_) => }
    }
}
