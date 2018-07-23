package inferium.utils

import scala.collection.mutable

object Utils {

    // todo: check if mapper is really needed
    def mergeMaps[K, V](maps: Map[K, V]*)(merge: (V, V) => V): Map[K, V] = {
        if (maps.isEmpty) {
            return Map.empty
        }

        maps.reduceLeft ((r, m) => m.foldLeft(r) {
            case (dict, (k, v)) => dict + (k -> (dict get k map { merge(_, v) } getOrElse v))
        })
    }
    def mergeMapsWithMapper[K, V](m1: Map[K, V], m2: Map[K, V])(mapper: V => V = (a: V) => a)(merge: (V, V) => V): Map[K, V] = {
        val res = m1.foldLeft(m2) {
            case (dict, (k, v)) => dict + (k -> (dict get k map { merge(_, v) } getOrElse mapper(v)))
        }

        // map the properties of m2
        m2.foldLeft(res) {
            case (dict, (k, v)) =>
                if (m1.contains(k))
                    dict
                else
                    dict + (k -> mapper(v))
        }
    }

    def mergeMaps[K, V](maps: mutable.Map[K, V]*)(merge: (V, V) => V): mutable.Map[K, V] = {
        val result = maps.head.clone()

        maps.tail foreach {
            m => m foreach {
                case (k, v) =>
                    result += k -> (result get k map { merge(_, v) } getOrElse v)
            }
        }
        result
    }

    implicit class UtilsString(val str: String) extends AnyVal {
        def splitCamelCase: Seq[String] = {
            return str.replaceAll(
                String.format("%s|%s|%s",
                    "(?<=[A-Z])(?=[A-Z][a-z])",
                    "(?<=[^A-Z])(?=[A-Z])",
                    "(?<=[A-Za-z])(?=[^A-Za-z])"
                ),
                "%"
            ).split("%")
        }

        def ?: (cond: Boolean): String = if (cond) str else ""
    }

    implicit class UtilsBoolean(val left: Boolean) extends AnyVal {
        @inline
        def implies(conclusion: => Boolean): Boolean = !left || conclusion

        @inline
        def ==>(conclusion: => Boolean): Boolean = implies(conclusion)

        @inline
        def =/>(right: => Boolean): Boolean = !left || !right
    }
}
