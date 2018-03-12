package inferium.memory

import java.util.concurrent.atomic.AtomicInteger

import inferium.lattice.Entity
import inferium.memory.Heap.HeapMap

import scala.collection.mutable

class Heap(val depth: Int = 0, val prev: Option[Heap] = None, private val handles: HeapMap = mutable.Map.empty) {
    private var numChildren: AtomicInteger = new AtomicInteger(0)
    private[this] var corrupt = false

    private def destroy(): HeapMap = {
        assert(!corrupt)
        assert(modifiable)
        corrupt = true
        prev.foreach(_.numChildren.getAndDecrement())
        return handles
    }

    private def destroyIfModifiable(): HeapMap = {
        assert(!corrupt)
        if (modifiable) {
            return destroy()
        } else {
            return handles.clone()
        }
    }

    def isCorrupt: Boolean = corrupt
    def modifiable: Boolean = {
        assert(!corrupt)
        return numChildren.get() == 0
    }

    def root: Heap = prev.map(_.root).getOrElse(this)

    def apply(handle: Handle): Entity = readHandle(handle)

    def readHandle(handle: Handle): Entity = getHandle(handle).getOrElse(throw new IllegalArgumentException(s"Handle $handle not defined"))

    private def getHandle(handle: Handle): Option[Entity] = {
        assert(!corrupt)
        lazy val parentResult = prev.flatMap(_.getHandle(handle))
        return handles.get(handle).map(Some(_)).getOrElse(parentResult)
    }

    def writeHandle(handle: Handle, entity: Entity): Entity = {
        assert(!corrupt)
        assert(modifiable)

        handles.put(handle, entity)
        return entity
    }

    def split(): Heap = {
        assert(!corrupt)
        numChildren.getAndIncrement()
        return new Heap(depth + 1, Some(this))
    }

    def unify(other: Heap): Heap = Heap.unify(Seq(this, other))
}


object Heap {
    type HeapMap = mutable.Map[Handle, Entity]

    def split(heap: Heap): Traversable[Heap] = Stream.continually(heap.split())
    def split(heap: Heap, moreThanOneNeeded: Boolean): Traversable[Heap] = {
        if (moreThanOneNeeded)
            split(heap)
        else
            heap #:: Stream.continually[Heap](throw new IllegalStateException("Did not expect multiple splits"))
    }
    def split(heap: Heap, splitsNeeded: Int): Traversable[Heap] = split(heap, splitsNeeded > 1)

    private class Unifier(startHeap: Heap) {
        var nextHeap: Option[Heap] = startHeap.prev
        var objects: HeapMap = startHeap.destroy()

        def depth: Int = nextHeap.map(_.depth).getOrElse(0)
        def canUp: Boolean = nextHeap.isDefined

        def up(): Unit = {
            assert(canUp)
            val Some(heap) = nextHeap
            for (pair@(obj, _) <- heap.handles) {
                if (!objects.contains(obj)) {
                    objects += pair
                }
            }
            nextHeap = heap.prev
        }

        def merge(other: Unifier): Unit = {
            val a = this
            val b = other
            if (a.objects.size < b.objects.size) {
                b.merge(a)
                return
            }
            val objSet = a.objects.keySet | b.objects.keySet
            val result = a.objects

            objSet.foreach {
                handle =>
                    lazy val aDefault = a.nextHeap.flatMap(_.getHandle(handle))
                    lazy val bDefault = b.nextHeap.flatMap(_.getHandle(handle))
                    val aVal = a.objects.get(handle).map(Some(_)).getOrElse(aDefault)
                    val bVal = b.objects.get(handle).map(Some(_)).getOrElse(bDefault)

                    bVal.map(bv => aVal.map(av => av.unify(bv)).getOrElse(bv)).foreach(a.objects.put(handle, _))
            }
            b.objects = a.objects
        }
    }

    def unify(heaps: Seq[Heap]): Heap = {
        assert(heaps.nonEmpty)
        assert(heaps.forall(!_.isCorrupt))
        assert(heaps.forall(_.modifiable))
        assert(heaps.distinct.length == heaps.length)
        assert(heaps.map(_.root).distinct.length == 1)

        heaps match {
            case Seq(mem) =>
                return mem
            case _ =>
        }

        val unifiers = mutable.Map.empty[Option[Heap], Unifier]
        // the greatest depth must be front!
        val queue = mutable.PriorityQueue.empty[Unifier](Ordering.by(_.depth))

        def addToUnifiers(unifier: Unifier): Unit = {
            unifiers.get(unifier.nextHeap) match {
                case Some(equalUnifier) =>
                    equalUnifier.merge(unifier)
                case None =>
                    unifiers += (unifier.nextHeap -> unifier)
                    queue += unifier
            }
        }

        heaps.map(new Unifier(_)).foreach(addToUnifiers)

        while(true) {
            val unifier = queue.dequeue()
            unifiers -= unifier.nextHeap

            if (queue.isEmpty) {
                assert(unifiers.isEmpty)
                if (unifier.canUp && unifier.nextHeap.get.numChildren.get == 0) {
                    unifier.up()
                }
                return new Heap(unifier.depth, unifier.nextHeap, unifier.objects)
            }

            unifier.up()
            addToUnifiers(unifier)
        }

        throw new IllegalStateException("should not be reached")
    }
}
