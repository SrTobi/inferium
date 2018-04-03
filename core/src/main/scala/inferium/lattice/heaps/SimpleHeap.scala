package inferium.lattice.heaps

import inferium.lattice.{Heap, Location, ObjLocation}

class SimpleHeap(private var entries: Map[ObjLocation, _] = Map.empty) extends Heap {

    override def begin(location: Location): Heap.HeapMutator = ???

    override def end(actor: Heap.HeapMutator): Heap = ???

    override def split(): Heap = this

    override def unify(heaps: Seq[Heap]): Heap = new SimpleHeap()

    override def fixpointUnify(futureHeap: Heap): Heap = ???
}
