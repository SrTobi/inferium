package inferium

import inferium.Config._
import inferium.dataflow.GraphBuilder
import org.scalatest.{FlatSpec, Matchers}

class ConfigSpec extends FlatSpec with Matchers {
    object TestSection extends Config.Section("TestSection") {
        val intOption: ConfigKey[Int] = ConfigKey(0)
        val stringOption: ConfigKey[String] = ConfigKey("")
    }

    object TestConfig extends Config.Definition {
        override val sections: Seq[Section] = Seq(TestSection)
    }

    import TestSection._

    "Config key" should "have correct name" in {
        intOption should have (
            'section ("TestSection"),
            'name ("intOption"),
            'fullName ("TestSection.intOption")
        )

        stringOption should have (
            'section ("TestSection"),
            'name ("stringOption"),
            'fullName ("TestSection.stringOption")
        )
    }

    it should "save default values" in {
        intOption.default shouldBe 0
        stringOption.default shouldBe ""
    }

    it should "parse string" in {
        intOption.parse("1234").value shouldBe 1234
        stringOption.parse("Test").value shouldBe "Test"
    }

    "Config" should "be comparable" in {
        Config() shouldBe new Config()


        val config1 = Config(intOption := 9)
        val config2 = Config()

        config1 shouldNot be (config2)

        config2.set(intOption := 9)

        config1 shouldBe config2
    }

    it should "return correct values for keys" in {
        val config = Config(intOption := 10)

        config(intOption) shouldBe 10
        config(stringOption) shouldBe ""

        config.set(stringOption := "test")

        config(stringOption) shouldBe "test"
    }

    it should "merge correctly" in {
        val config = Config(intOption := 11, stringOption := "xxx")

        config <+= Config(intOption := 12)

        config(intOption) shouldBe 12
        config(stringOption) shouldBe "xxx"
    }

    it should "correctly set default" in {
        val config = Config(stringOption := "test")
        config(stringOption) shouldBe "test"

        config.set(stringOption := Default)

        config(stringOption) shouldBe ""
    }

    it should "be queryable for sections" in {
        TestConfig.section("TestSection") shouldBe TestSection
        TestConfig.section("test-section") shouldBe TestSection
    }

    it should "be queryable for keys" in {
        TestConfig.key("TestSection.intOption") shouldBe TestSection.intOption
        TestConfig.key("Test-Section.int-option") shouldBe TestSection.intOption
        TestConfig.key("TestSection.string-option") shouldBe TestSection.stringOption
        TestConfig.key("test-section.stringoption") shouldBe TestSection.stringOption
    }

    it should "parse config lists" in {
        val configSource = Seq(
            "test-section.int-option" -> "234",
            "testSection.stringOption" -> "Hello"
        )

        val ref = Config(
            intOption := 234,
            stringOption := "Hello"
        )

        val parsed = TestConfig.parse(configSource)
        parsed shouldBe ref
    }

    it should "parse config scripts" in {
        val ref = Config(
            intOption := 234,
            stringOption := "Hello test"
        )

        {
            val configSource =
                """
                  |testSection.intoption: 234
                  |testSection.string-option: "Hello test"
                """.stripMargin
            val parsed = TestConfig.parse(configSource)
            parsed shouldBe ref
        }

        {
            val configSource =
                """
                  |test-section {
                  |  intOption: "234"
                  |  string-option: Hello test
                  |}
                """.stripMargin
            val parsed = TestConfig.parse(configSource)
            parsed shouldBe ref
        }
    }

    "GraphBuilder section" should "have all options" in {
        import GraphBuilder.Config._
        val ref = GraphBuilder.Config(bindLetAndConstToGlobal = true)
        val empty = Config()
        val config = Config(bindLetAndConstToGlobal := ref.bindLetAndConstToGlobal)

        // check that config does not contain default values
        empty(bindLetAndConstToGlobal) shouldNot be (config(bindLetAndConstToGlobal))

        val fromConfig: GraphBuilder.Config = config
        fromConfig shouldBe ref
    }
}
