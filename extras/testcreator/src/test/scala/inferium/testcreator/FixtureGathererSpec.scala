package inferium.testcreator

import java.nio.file.{Path, Paths}

import org.scalatest.{FlatSpec, Inside, Matchers}

class FixtureGathererSpec extends FlatSpec with Matchers with Inside {

    "FixtureGatherer" should "gather all files in test folder" in {
        val root = FixtureGatherer.gatherFixtures(Paths.get("samples"))

        root.filename shouldBe "/"
        root.path shouldBe "/"
        root.fixtures should have size 1

        {
            val fixtureFile = root.fixtures.head
            val fixture = fixtureFile.fixture
            fixtureFile.filename shouldBe "test.js"
            fixtureFile.path shouldBe "/test.js"

            fixture.name shouldBe "Hi"
            fixture.description shouldBe "Ich bin ein test"
        }

        root.subdirs should have size 1

        {
            val testdir = root.subdirs.head
            testdir.filename shouldBe "testdir"
            testdir.path shouldBe "/testdir"
            testdir.fixtures shouldBe 'empty
            testdir.subdirs should have size 1

            val second = testdir.subdirs.head
            second.filename shouldBe "second"
            second.path shouldBe "/testdir/second"
            second.subdirs shouldBe 'empty

            second.fixtures should have size 1

            {
                val fixtureFile = second.fixtures.head
                val fixture = fixtureFile.fixture
                fixtureFile.filename shouldBe "blub.js"
                fixtureFile.path shouldBe "/testdir/second/blub.js"

                fixture.name shouldBe "blub"
                fixture.description shouldBe "1234"
            }
        }

        root.allFixtures should have length 2
    }
}
