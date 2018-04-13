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
        Gen.chooseNum(-1000, 1000).map(SpecificNumberValue.apply),
        singletonPrimitives.map(Gen.const): _*
    )

    private val latticeMemberSeqGen = Gen.containerOf[Seq, Entity](latticeMemberGen)

    property("union(a, a) == a") {
        forAll(latticeMemberGen, minSuccessful(1000)) {
            (entity) =>
                val union = Entity.unify(Seq(entity, entity, entity))
                union shouldBe entity
        }
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
        forAll (latticeMemberSeqGen, minSuccessful(500)) {
            (entities) =>
                val union = Entity.unify(entities)

                entities foreach { entity =>
                    assert(union mightBe entity, s"$union might not be $entity")
                }
        }
    }
}
