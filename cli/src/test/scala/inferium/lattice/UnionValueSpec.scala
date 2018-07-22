package inferium.lattice

import org.scalacheck.Gen
import org.scalatest.{Assertions, Inspectors, Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class UnionValueSpec extends PropSpec with Assertions with Matchers with GeneratorDrivenPropertyChecks {

    private val singletonPrimitives = Seq(
        NeverValue,
        UndefinedValue,
        NullValue,

        // bool
        BoolValue,
        TrueValue,
        FalseValue,

        // number
        NumberValue,

        // string
        StringValue,
    )

    private val latticeMemberGen = Gen.oneOf(
        Gen.alphaNumStr.map(SpecificStringValue.apply),
        Gen.chooseNum(-1000L, 1000L).map(SpecificNumberValue.apply),
        singletonPrimitives.map(Gen.const): _*
    )

    private val latticeMemberSeqGen = Gen.containerOf[Seq, Entity](latticeMemberGen)

    property("union(a, a) == a") {
        forAll(latticeMemberGen, Gen.chooseNum(1, 5), minSuccessful(1000)) {
            (entity, times) =>
                val union = Entity.unify(Seq.fill(times){ entity })
                union shouldBe entity
        }

        (TrueValue unify FalseValue) shouldBe BoolValue
        (TrueValue unify BoolValue) shouldBe BoolValue
        (BoolValue unify FalseValue) shouldBe BoolValue

        (SpecificNumberValue(3) unify SpecificNumberValue(10)) shouldBe NumberValue
        (SpecificNumberValue(3) unify NumberValue) shouldBe NumberValue
        (NumberValue unify SpecificNumberValue(3)) shouldBe NumberValue

        (StringValue unify SpecificStringValue("test")) shouldBe StringValue
        (SpecificStringValue("xxx") unify SpecificStringValue("blub") unify StringValue) shouldBe StringValue

    }

    property("Single Entity.unify and multiple unifies create the same entity") {
        forAll (latticeMemberSeqGen, minSuccessful(500)) {
            (entities) =>
                val unionFromSingeUnify = Entity.unify(entities)
                val unionFromMultipleUnifies = entities.foldLeft[Entity](NeverValue) { _ unify _ }

                unionFromSingeUnify shouldBe unionFromMultipleUnifies
        }
    }

    property("mightBe") {
        forAll(latticeMemberGen, minSuccessful(500)) {
            (entity) =>
                assert(entity mightBe entity)
                assert(entity mightBe NeverValue)
        }

        forAll (latticeMemberSeqGen, minSuccessful(500)) {
            (entities) =>
                val union = Entity.unify(entities)

                entities foreach { entity =>
                    assert(union mightBe entity, s"$union might not be $entity")
                }
        }

        forAll(latticeMemberSeqGen, latticeMemberSeqGen, minSuccessful(500)) {
            (fst, snd) =>
                val subUnion = Entity.unify(fst)
                val superUnion = Entity.unify(fst ++ snd)
                assert(superUnion mightBe subUnion)
        }
    }
}
