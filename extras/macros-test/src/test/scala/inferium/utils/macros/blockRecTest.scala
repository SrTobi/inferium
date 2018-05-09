package inferium.utils.macros

import org.scalatest.{FlatSpec, Matchers}

class blockRecTest extends FlatSpec with Matchers {

    abstract class Base {
        var ref: Base = _

        @blockRec
        def test1(): String

        @blockRec
        def test2(): String
    }

    class TestWithRec(name: String) extends Base {
        @blockRec(default = "default")
        override def test1(): String = s"($name:${this.test2()}+${ref.test2()})"

        @blockRec("proxy")
        override def test2(): String = test1()
    }

    class TestWithoutRec(name: String) extends Base {
        @blockRec(nonrec = true)
        override def test1(): String = ref.test2()

        @blockRec(nonrec = true)
        override def test2(): String = s"(top:$name)"
    }

    "blockRec" should "prevent recursion for itself" in {
        val o = new TestWithRec("o")
        o.ref = o
        o.test1() shouldBe "(o:default+default)"
        o.test2() shouldBe "(o:proxy+proxy)"
    }

    it should "prevent recursion over multiple instances" in {
        val o1 = new TestWithRec("o1")
        val o2 = new TestWithRec("o2")
        o1.ref = o2
        o2.ref = o1

        o1.test1() shouldBe "(o1:default+(o2:proxy+default))"
        o1.test2() shouldBe "(o1:proxy+(o2:proxy+proxy))"
        o2.test1() shouldBe "(o2:default+(o1:proxy+default))"
        o2.test2() shouldBe "(o2:proxy+(o1:proxy+proxy))"
    }

    it should "prevent recursion with non recursive classes" in {
        val o1 = new TestWithRec("o1")
        val o2 = new TestWithoutRec("o2")
        o1.ref = o2
        o2.ref = o1

        o1.test1() shouldBe "(o1:default+(top:o2))"
        o1.test2() shouldBe "(o1:proxy+(top:o2))"
        o2.test1() shouldBe "(o1:proxy+(top:o2))"
        o2.test2() shouldBe "(top:o2)"
    }
}
