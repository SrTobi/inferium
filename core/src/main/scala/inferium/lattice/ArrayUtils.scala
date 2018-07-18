package inferium.lattice

import inferium.dataflow.ExecutionState
import inferium.dataflow.graph.traits.HeapReading

object ArrayUtils {

    def fillArray(array: ObjectLike,
                  elements: Seq[Entity],
                  restElement: Option[Entity],
                  mutator: Heap.Mutator,
                  propertyWriteLocations: Iterator[Location]): Unit = {

        restElement foreach {
            mutator.writeToProperties(array, null, numbersOnly = true, _)
        }

        val lengthLocation = propertyWriteLocations.next()
        for ((element, idx) <- elements.zipWithIndex) {
            if (element != UndefinedValue) {
                assert(propertyWriteLocations.hasNext)
                mutator.forceSetPropertyValue(array, idx.toString, propertyWriteLocations.next(), element)
            }
        }

        val length = if (restElement.isDefined) NumberValue else SpecificNumberValue(elements.length)
        mutator.forceSetPropertyValue(array, "length", lengthLocation, length)
    }

    def fillAbstractArray(array: ObjectLike,
                          elements: Seq[Entity],
                          restElement: Option[Entity],
                          mutator: Heap.Mutator): Unit = {

        restElement foreach {
            mutator.writeToProperties(array, null, numbersOnly = true, _)
        }

        for ((element, idx) <- elements.zipWithIndex) {
            if (element != UndefinedValue) {
                mutator.forceSetAbstractPropertyValue(array, idx.toString, element)
            }
        }

        val length = if (restElement.isDefined) NumberValue else SpecificNumberValue(elements.length)
        mutator.forceSetAbstractPropertyValue(array, "length", length)
    }

}
