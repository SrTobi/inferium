package inferium.testcreator.cli

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import inferium.InferiumConfig
import inferium.testcreator.{FixtureGatherer, FixtureTestBuilder}

object BundleFixtures {
    def main(args: Array[String]): Unit = {

        args match {
            case Array(inputDir, targetRoot, packageName, name) =>
                println(s"Name is $packageName.$name")
                val targetFile = Paths.get(targetRoot, packageName.split('.').toSeq :+ s"$name.scala": _*)

                println(s"Read from ${Paths.get(inputDir).toAbsolutePath}")
                val dir = FixtureGatherer.gatherFixtures(inputDir, InferiumConfig.Env.NodeDebug)

                println(s"${dir.allFixtures.length} fixtures found")

                println("building...")
                val testContent = new FixtureTestBuilder().buildTest(packageName, name, dir)

                println(s"Write to  ${targetFile.toAbsolutePath}")
                Files.write(targetFile, testContent.getBytes(StandardCharsets.UTF_8))

            case _ =>
                println("testcreator <input-dir> <target-dir> <package-name> <name>")
        }
    }
}
