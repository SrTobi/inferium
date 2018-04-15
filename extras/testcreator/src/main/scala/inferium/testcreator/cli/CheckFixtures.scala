package inferium.testcreator.cli

import java.nio.file.{Files, Paths}

import inferium.InferiumConfig
import inferium.testcreator.{FixtureGatherer, FixtureTestBuilder}
import inferium.testtools.FixtureChecker

import scala.util.{Failure, Success}

object CheckFixtures {
    def main(args: Array[String]): Unit = {

        args match {
            case Array(fixtureDir) =>
                println(s"Read from ${Paths.get(fixtureDir).toAbsolutePath}")
                val dir = FixtureGatherer.gatherFixtures(fixtureDir, InferiumConfig.Env.NodeDebug)

                println(s"${dir.allFixtures.length} fixtures found")

                val checker = new FixtureChecker
                dir.allFixtures foreach {
                    fixture =>
                        println(s"Check fixture ${fixture.path}")
                        checker.check(fixture.content) match {
                            case Success(true) =>
                            case Success(false) =>
                                System.err.println(" => Script did not return expected \"ok\" string")
                            case Failure(e) =>
                                System.err.println(s" => $e")
                        }
                }

            case _ =>
                println("testcreator <fixture-dir>")
        }
    }
}
