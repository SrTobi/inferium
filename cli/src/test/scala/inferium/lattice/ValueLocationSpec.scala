package inferium.lattice

import org.scalatest.{FlatSpec, Matchers}

class ValueLocationSpec extends FlatSpec with Matchers {

    "ValueLocation" should "handle bits correctly" in {
        val v1 = new ValueLocation(Location.create(Int.MaxValue), 0)
        v1.loc shouldEqual Location.create(Int.MaxValue)
        v1.abstractCount shouldEqual 0

        val v2 = v1.withAbstractCount(Int.MaxValue)
        v2.loc shouldEqual Location.create(Int.MaxValue)
        v2.abstractCount shouldEqual Int.MaxValue

        val v3 = new ValueLocation(Location.create(Int.MinValue), 0)
        v3.loc shouldEqual Location.create(Int.MinValue)
        v3.abstractCount shouldEqual 0

        val v4 = v1.withAbstractCount(Int.MinValue)
        v4.loc shouldEqual Location.create(Int.MaxValue)
        v4.abstractCount shouldEqual Int.MinValue
    }
}
