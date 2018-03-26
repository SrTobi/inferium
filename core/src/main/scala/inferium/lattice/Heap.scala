package inferium.lattice

import inferium.lattice.Heap.HeapMutator

abstract class Heap {
    def begin(location: Location): HeapMutator
    def end(actor: HeapMutator): Heap

    def split(): Heap
    def unify(heap: Heap): Heap = unify(Seq(heap))
    def unify(heaps: Seq[Heap]): Heap

    def fixpointUnify(futureHeap: Heap): Heap
}


object Heap {
    abstract class HeapMutator {
        def createObject(location: Location): ObjLocation
        def writeProperty(obj: Entity, property: String, value: Entity): Entity
        def readProperty(obj: Entity, property: String): Entity
        //def listProperties(obj: ObjLocation): Seq[Entity]
    }

    abstract class Factory {
        def create(): Heap
    }
}