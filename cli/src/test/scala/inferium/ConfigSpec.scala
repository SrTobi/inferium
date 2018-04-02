package inferium

import inferium.Config._
import inferium.dataflow.GraphBuilder
import org.scalatest.{FlatSpec, Matchers}

class ConfigSpec extends FlatSpec with Matchers {
    object TestSection {
        val intOption: ConfigKey[Int] = ConfigKey(0)
        val stringOption: ConfigKey[String] = ConfigKey("")
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
