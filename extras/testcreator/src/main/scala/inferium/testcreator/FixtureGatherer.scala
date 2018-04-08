package inferium.testcreator

import inferium.debug.Fixture

import scala.collection.immutable.ListSet
import java.nio.file.{Files, Path, Paths}

import inferium.{Config, InferiumConfig}

import scala.collection.mutable



object FixtureGatherer {
    abstract class PathEntity(path: String) {
        val filename: String = if (path == "/") "/" else Paths.get(path).getFileName.toString
    }

    case class FixtureFile(path: String)(val fixture: Fixture) extends PathEntity(path)

    case class Directory(path: String)(_subdirs: Seq[Directory], _fixtures: Seq[FixtureFile]) extends PathEntity(path) {
        val subdirs = ListSet(_subdirs.sortBy(_.filename): _*)
        val fixtures = ListSet(_fixtures.sortBy(_.filename): _*)

        def allFixtures: Seq[FixtureFile] = _fixtures.view ++ _subdirs.view.flatMap{ _.allFixtures }
    }

    def readFixture(path: Path): Fixture = {
        val content = scala.io.Source.fromFile(path.toFile).mkString

        val commentBegin = content.indexOf("/*")
        require(commentBegin >= 0, "Could not find prolog begin")

        val commentEnd = content.indexOf("*/", commentBegin)
        require(commentBegin >= 0, "Could not find prolog end")

        val prolog = content.substring(commentBegin + 2, commentEnd - commentBegin - 2)
        val code = content.substring(commentEnd + 2)

        val (config, localSettings) = InferiumConfig.parseWithFreeEntries(prolog)

        val name = localSettings getOrElse("name", throw new Exception("Fixture prolog does not specify a name"))
        val description = localSettings getOrElse("desc", throw new Exception("Fixture prolog does not specify a description"))

        Fixture(name, description, config, code)
    }

    def gatherFixtures(_rootPath: Path): Directory = {
        val rootPath = _rootPath.toAbsolutePath
        require(Files.isDirectory(rootPath))

        def rel(p: Path): String = "/" + rootPath.relativize(p).toString

        def readFixtureFile(path: Path): FixtureFile = {
            try {
                val fixture = readFixture(path)
                FixtureFile(rel(path))(fixture)
            } catch {
                case e: Exception =>
                    throw new Exception(s"Failed to read fixture '$path'", e)
            }
        }

        def gatherRecursive(path: Path): Directory = {

            val dirs = mutable.Buffer.empty[Directory]
            val fixtures = mutable.Buffer.empty[FixtureFile]

            Files.newDirectoryStream(path).forEach {
                entry =>
                    if (Files.isRegularFile(entry)) {
                        fixtures += readFixtureFile(entry)
                    } else {
                        dirs += gatherRecursive(entry)
                    }
            }

            Directory(rel(path))(dirs, fixtures)
        }

        gatherRecursive(rootPath)
    }

    def gatherFixtures(path: String): Directory = gatherFixtures(Paths.get(path))
}
