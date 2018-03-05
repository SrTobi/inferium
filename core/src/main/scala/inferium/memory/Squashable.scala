package inferium.memory

import scala.collection.mutable

abstract class Squashable[T, Key, Value] {
    this: T =>

    private val member = mutable.HashMap.empty[Key, Value]

    protected def mergeValues(left: Value, right: Value): Value

}
