package inferium.memory

import inferium.lattice._
import org.scalatest.{FlatSpec, Matchers}

class HeapTest extends FlatSpec with Matchers {

    "A Heap" should "read the same value that was written" in {
        val heap = new Heap
        val h = new Handle
        val h2 = new Handle

        heap.writeHandle(h, TrueValue)
        heap.writeHandle(h2, SpecificNumberValue(2))
        heap.readHandle(h) shouldBe TrueValue

        heap.writeHandle(h, FalseValue)
        heap.readHandle(h) shouldBe FalseValue
        heap.readHandle(h2) shouldBe SpecificNumberValue(2)
    }

    it should "read the same value that was written even if it was split" in {

        val heap = new Heap
        val h = new Handle

        heap.writeHandle(h, TrueValue)
        heap.readHandle(h) shouldBe TrueValue

        val heap2 = heap.split()
        heap2.readHandle(h) shouldBe TrueValue
        heap2.writeHandle(h, FalseValue)
        heap2.readHandle(h) shouldBe FalseValue

        heap.readHandle(h) shouldBe TrueValue
    }

    it should "unify handle values from different heaps when unified" in {
        val base = new Heap
        val h = new Handle

        val h1 = base.split()
        val h2 = base.split()

        h1.writeHandle(h, TrueValue)
        h2.writeHandle(h, FalseValue)

        val merged = h1.unify(h2)
        merged.readHandle(h) shouldBe BoolValue
    }

    it should "respect handle values from previous heaps if one unification heap does not contain changes to a handle" in {

        val base = new Heap
        val h = new Handle

        base.writeHandle(h, FalseValue)

        val h1 = base.split()
        val h2 = base.split()

        h1.writeHandle(h, TrueValue)

        val merged = h1.unify(h2)
        merged.readHandle(h) shouldBe BoolValue
    }
}
