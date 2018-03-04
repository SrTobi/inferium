package inferium.lattice


sealed abstract class GeneralBool {

    def unify(other: GeneralBool): GeneralBool
}

sealed abstract class Bool extends GeneralBool {
    import Bool._

    def unify(other: GeneralBool): Bool = if (this == other || other == GeneralBool.Bottom) this else Top
    def unify(other: Bool): Bool = if (this == other) this else Top
}

object Bool {
    case object Top extends Bool
    case object True extends Bool
    case object False extends Bool

    def apply(value: Boolean): Bool = if (value) True else False

    def unify(bools: Iterable[Bool]): Bool = {
        val it = bools.iterator
        var result: Bool = it.next()
        for (bool <- it) {
            result = result.unify(bool)
            if (result == Bool.Top) {
                return Bool.Top
            }
        }
        return result
    }
}

object GeneralBool {
    case object Bottom extends GeneralBool {
        override def unify(other: GeneralBool): GeneralBool = other
    }
}