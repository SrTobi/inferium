package inferium.lattice

import org.scalatest.{FlatSpec, Inspectors, Matchers}

class PrimitivesTest extends FlatSpec with Matchers with Inspectors {

    private def primitives: Seq[Primitive] = Seq(
        NeverValue,
        UndefinedValue,
        NullValue,

        // bool
        BoolValue,
        TrueValue,
        FalseValue,

        // number
        NumberValue,
        SpecificNumberValue(3),
        SpecificNumberValue(-88),

        // string
        StringValue,
        SpecificStringValue(""),
        SpecificStringValue("test"),
        SpecificStringValue("3")
    )

    "A primitive" should "only be equal to itself" in {
        forAll(primitives) { p =>
            p shouldEqual p
            exactly (1, primitives) shouldEqual p
        }
    }

    it should "return a nice string for toString" in {
        NeverValue.toString shouldEqual "never"
        UndefinedValue.toString shouldEqual "undefined"
        NullValue.toString shouldEqual "null"

        BoolValue.toString shouldEqual "boolean"
        TrueValue.toString shouldEqual "true"
        FalseValue.toString shouldEqual "false"

        NumberValue.toString shouldEqual "number"
        SpecificNumberValue(3).toString shouldEqual "3"

        StringValue.toString shouldEqual "string"
        SpecificStringValue("test").toString shouldEqual "\"test\""
    }
}
