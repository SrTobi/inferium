package inferium.testcreator

import inferium.testcreator.FixtureGatherer.{Directory, FixtureFile}

import scala.collection.mutable

class FixtureTestBuilder {
    private var done = false
    private val builder = new mutable.StringBuilder

    private def buildTestCase(fixtureFile: FixtureFile): Unit = {
        val fixture = fixtureFile.fixture
        builder.append("        \"" + fixture.description + "\" in {\n")

        builder.append("            val code =\n")
        builder.append("                \"\"\"")

        fixtureFile.content.split("\n").zipWithIndex foreach {
            case (line,  idx) =>
                if (idx > 0)
                    builder.append("                  |")
                builder.append(line)
                builder.append("\n")
        }

        builder.append("                \"\"\".stripMargin\n")
        builder.append(
            """
              |            FixtureRunner.test(code)
              |        }
            """.stripMargin)
        builder.append("\n")
    }

    private def buildDir(dir: Directory): Unit = {

        if (dir.fixtures.nonEmpty) {
            builder.append("    \"" + dir.path + "\" - {\n")
            dir.fixtures foreach buildTestCase
            builder.append("    }\n\n\n")
        }

        dir.subdirs foreach buildDir
    }

    def buildTest(pkg: String, name: String, dir: Directory): String = {
        require(!done)

        val prolog =
            s"""
              |package $pkg
              |
              |import inferium.debug.{Fixture, FixtureRunner}
              |import org.scalatest.{FreeSpec, Matchers}
              |
              |class $name extends FreeSpec with Matchers {
            """.stripMargin

        val epilog = "}\n"


        builder.append(prolog)
        builder.append("\n")
        buildDir(dir)
        builder.append(epilog)
        done = true

        return toString
    }

    override def toString: String = {
        require(done)
        builder.toString()
    }
}
