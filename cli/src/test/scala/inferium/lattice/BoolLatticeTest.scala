package inferium.lattice

import org.scalatest.{FlatSpec, Matchers}

class BoolLatticeTest extends FlatSpec with Matchers {
    import BoolLattice.{False, Top, True}
    import GeneralBoolLattice.Bottom

    "A Bool lattice" should "be constructable from Boolean" in {
        BoolLattice(true) shouldBe True
        BoolLattice(false) shouldBe False
    }

    it should "handle unification of equal values correct" in {
        True.unify(True) shouldBe True
        False.unify(False) shouldBe False
        Top.unify(Top) shouldBe Top
        Bottom.unify(Bottom) shouldBe Bottom
    }

    "Top" should "alsways unify to top" in {
        Top.unify(True) shouldBe Top
        True.unify(Top) shouldBe Top

        Top.unify(False) shouldBe Top
        False.unify(Top) shouldBe Top

        Top.unify(Bottom) shouldBe Top
        Bottom.unify(Top) shouldBe Top
    }

    it should "be the result of the unification of true and false" in {
        True.unify(False) shouldBe Top
        False.unify(True) shouldBe Top
    }

    "Bottom" should "unified with true or false always result in true or false respectively" in {
        Bottom.unify(True) shouldBe True
        True.unify(Bottom) shouldBe True

        Bottom.unify(False) shouldBe False
        False.unify(Bottom) shouldBe False
    }

    "Bool.unify" should "return bool X when multiple X are unified" in {
        BoolLattice.unify(Seq(True, True, True, True, True)) shouldBe True
        BoolLattice.unify(Seq(False, False, False, False, False)) shouldBe False
        BoolLattice.unify(Seq(Top, Top, Top, Top, Top)) shouldBe Top
    }

    it should "return Top if different lattice member are unified" in {
        BoolLattice.unify(Seq(True, False, True)) shouldBe Top
        BoolLattice.unify(Seq(False, False, True)) shouldBe Top
        BoolLattice.unify(Seq(False, False, Top)) shouldBe Top
        BoolLattice.unify(Seq(Top, False, True)) shouldBe Top
        BoolLattice.unify(Seq(Top, Top, True)) shouldBe Top
        BoolLattice.unify(Seq(True, True, False)) shouldBe Top
    }
}
