package inferium.utils

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class UtilsSpec extends FlatSpec with Matchers {
    "mergeMaps" should "merge immutable maps" in {
        val map1 = Map(1 -> "no merge", 2 -> "merge")
        val map2 = Map(2 -> "merge", 3 -> "no merge")

        val merged = Utils.mergeMaps(map1, map2)() { _ + _ }

        merged(1) shouldBe "no merge"
        merged(2) shouldBe "mergemerge"
        merged(3) shouldBe "no merge"
        merged.size shouldBe 3
    }

    it should "merge mutable maps" in {
        val map1 = mutable.Map(1 -> "no merge", 2 -> "merge")
        val map2 = mutable.Map(2 -> "merge", 3 -> "no merge")

        val merged = Utils.mergeMaps(map1, map2) { _ + _ }

        merged(1) shouldBe "no merge"
        merged(2) shouldBe "mergemerge"
        merged(3) shouldBe "no merge"
        merged.size shouldBe 3
    }
}
