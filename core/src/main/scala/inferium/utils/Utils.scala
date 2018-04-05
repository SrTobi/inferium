package inferium.utils

import scala.collection.mutable

object Utils {

    def mergeMaps[K, V](maps: Map[K, V]*)(merge: (V, V) => V): Map[K, V] =
        maps.reduceLeft ((r, m) => m.foldLeft(r) {
            case (dict, (k, v)) => dict + (k -> (dict get k map { merge(_, v) } getOrElse v))
        })

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

    implicit class UtilsString(str: String) extends AnyRef {
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

    }
}
