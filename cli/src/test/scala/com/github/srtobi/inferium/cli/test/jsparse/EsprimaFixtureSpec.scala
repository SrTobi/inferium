package com.github.srtobi.inferium.cli.test.jsparse

import java.io.File

import com.github.srtobi.inferium.cli.jsparser.ECMAScript2018Parse
import fastparse.core.Parsed
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source


class EsprimaFixtureSpec extends FunSuite with Matchers {

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_ != null).filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  val fixturePath = "/esprima/test/fixtures/"

  {
    val esprimaFixtureDir = new File(getClass.getResource(fixturePath).getPath)
    esprimaFixtureDir shouldBe 'isDirectory
    val testFiles = recursiveListFiles(esprimaFixtureDir)

    testFiles
      .map(_.getPath)
      .filter(_.endsWith(".js"))
      .filter(!_.contains("invalid"))
  } foreach {
    testPath =>
      val file = new File(testPath)
      val path = testPath.substring(testPath.indexOf(fixturePath) + fixturePath.length)
      test(s"The file '$path' should be parsable") {
        file shouldBe 'exists
        val source = Source.fromFile(file).getLines.mkString("\n")
        println(source)
        val result = ECMAScript2018Parse.script.parse(source)
        result should matchPattern { case Parsed.Success(_, _) => }
      }
  }
}
