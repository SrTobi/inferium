package inferium.dataflow.graph

import inferium.dataflow.{CallableInfo, DataFlowAnalysis, ExecutionState}
import inferium.dataflow.graph.traits._
import inferium.lattice.Heap.SpecialObjects
import inferium.lattice._

class AllocateArrayNode(private val elements: Seq[Option[Boolean]])(implicit _info: Node.Info) extends LinearNode with SingleSuccessor with HeapWriting {

    private val heapAccessLoc: Location = Location()
    private val allocSite: Location = Location()

    private val spreadElements = elements map { _ getOrElse false }
    val elementsOnStackCount: Int = elements.count(_.isDefined)
    def elementsCount: Int = elements.length

    override def process(implicit analysis: DataFlowAnalysis): Unit = {
        val initialHeap = inState.heap
        val initialStack = inState.stack

        // get arguments
        val (reverseElements, restStack) = initialStack.splitAt(elementsOnStackCount)
        val stackElements = reverseElements.reverse
        assert(stackElements.length == elementsOnStackCount)

        val eIt = stackElements.iterator
        val elements = this.elements.map {
            case Some(_) => eIt.next()
            case None => UndefinedValue
        }

        assert(!eIt.hasNext)

        val mutator = initialHeap.begin(heapAccessLoc)
        val array = mutator.allocObject(allocSite, (loc, ac) => OrdinaryObjectEntity(loc)(ac), initialHeap.specialObject(SpecialObjects.Array))

        val heapAfterCreation = initialHeap.end(mutator)
        val stateAfterCreation = inState.copy(stack = restStack, heap = heapAfterCreation)

        spreading.array = array
        spreading.spread(elements, stateAfterCreation)
    }

    private object spreading extends SeqSpreader(spreadElements) {
        var array: ObjectLike = _

        private val heapAccessLoc: Location = Location()
        private val lengthWriteLocation = Location()
        private val elementsWriteLocations = Stream.continually(Location())

        override protected def onComplete(result: (Seq[Entity], Option[Entity]), state: ExecutionState, analysis: DataFlowAnalysis): Unit = {
            val initialHeap = state.heap
            val mutator = initialHeap.begin(heapAccessLoc)

            val (elements, restElement) = result

            restElement foreach {
                mutator.writeToProperties(array, null, numbersOnly = true, _)
            }

            val locIt = elementsWriteLocations.iterator
            for ((element, idx) <- elements.zipWithIndex) {
                if (element != UndefinedValue) {
                    assert(locIt.hasNext)
                    mutator.forceSetPropertyValue(array, idx.toString, locIt.next(), element)
                }
            }

            mutator.forceSetPropertyValue(array, "length", lengthWriteLocation, SpecificNumberValue(elementsCount))

            val resultHeap = initialHeap.end(mutator)
            val resultState = state.copy(stack = array :: state.stack, heap = resultHeap)

            succ.setNewInState(resultState, _thisOrigin)(analysis)
        }
    }

    override def asAsmStmt: String = s"pushArray $elementsOnStackCount -> $elementsCount"
}
