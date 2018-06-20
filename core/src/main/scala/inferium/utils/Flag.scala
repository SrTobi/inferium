package inferium.utils


abstract class Flag[T] {
    private var nextFlag: Int = 1

    type Value = Flag.Value[T]

    def Value: Value = {
        val temp = nextFlag
        nextFlag = nextFlag << 1
        assert(nextFlag > temp)
        new Value(temp)
    }

    def Empty: Value = new Value(0)
}

object Flag {
    final class Value[T] private[Flag] (val value: Int) extends AnyVal {
        def has(other: Value[T]): Boolean = (value & other.value) == other.value
        def &(other: Value[T]): Boolean = (value & other.value) != 0

        def |(other: Value[T]): Value[T] = new Value[T](other.value | value)

        def when(cond: Boolean): Value[T] = if (cond) this else new Value[T](0)
    }
}
