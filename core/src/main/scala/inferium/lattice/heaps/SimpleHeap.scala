package inferium.lattice.heaps

import inferium.Unifiable
import inferium.lattice.Heap.SpecialObjects.SpecialObject
import inferium.lattice.Heap.{Mutator, Shared, SpecialObjectMap}
import inferium.lattice._
import inferium.utils.Utils



object SimpleHeap extends Heap.Factory with HeapImmutables {
    override def create(config: Heap.Config): (Heap, SpecialObjectMap) = {
        val shared = new Heap.Shared(config)
        val heap = new SimpleHeapImpl(shared)
        (heap, shared.specialObjects)
    }

    private class SimpleHeapImpl(override val shared: Heap.Shared,
                                 val objects: Map[Location, Obj] = Map.empty,
                                 val boxedValues: Map[Location, BoxedValue] = Map.empty) extends Heap {


        private def createMutator(): SimpleMutator = new SimpleMutator(objects, boxedValues, shared)
        override def begin(location: Location): SimpleMutator = createMutator()

        override def end(actor: Heap.Mutator): Heap = {
            val mutator = actor.asInstanceOf[SimpleMutator]
            new SimpleHeapImpl(shared, mutator.objects, mutator.boxedValues)
        }

        override def unify(heaps: Seq[Heap])(implicit fixpoint: Unifiable.Fixpoint): Heap = {
            val allHeaps = (this +: heaps) map { _.asInstanceOf[SimpleHeapImpl] }
            // todo: this mutator is problematic, because it might not reference the correct heap
            val mutator = createMutator()
            val newObjects = Utils.mergeMaps(allHeaps map { _.objects }: _*) {
                case (Obj(ad1, cd1, c1), Obj(ad2, cd2, c2)) =>
                    val (abstractDesc, concreteDesc) = if (c1 < c2) {
                        val abstractified = abstractifyConcreteDesc(cd1, mutator)
                        val newAd1 = if (wasAbstractified(c1)) mergeAbsDesc(abstractified, ad1) else abstractified
                        mergeAbsDesc(newAd1, ad2) -> cd2
                    } else if (c1 > c2) {
                        val abstractified = abstractifyConcreteDesc(cd2, mutator)
                        val newAd2 = if (wasAbstractified(c2)) mergeAbsDesc(abstractified, ad2) else abstractified
                        mergeAbsDesc(ad1, newAd2) -> cd1
                    } else {
                        mergeAbsDesc(ad1, ad2) -> mergeConcreteDesc(cd1, cd2)
                    }

                    Obj(abstractDesc, concreteDesc, Math.max(c1, c2))
            }

            val newBoxedValues = Utils.mergeMaps(allHeaps map { _.boxedValues }: _*) { _ unify _}

            new SimpleHeapImpl(shared, newObjects, newBoxedValues)
        }

        override def equals(o: scala.Any): Boolean = o match {
            case other: SimpleHeapImpl =>
                objects == other.objects && boxedValues == other.boxedValues

            case _ =>
                false
        }

        override def createGlobalHeap(): GlobalHeap = new SimpleGlobalHeap(shared, objects, boxedValues)
    }

    private class SimpleGlobalHeap(private val shared: Shared,
                                   private var objects: Map[Location, Obj],
                                   private var boxedValues: Map[Location, BoxedValue]) extends GlobalHeap {

        override def hasEffect(location: Location): Boolean = true
        override def toHeap(location: Location): Heap = new SimpleHeapImpl(shared, objects, boxedValues)

        override def feed(heap: Heap): Boolean = {
            val other = heap.asInstanceOf[SimpleHeapImpl]
            val accessor = this.accessor
            val newObjects = Utils.mergeMaps(objects, other.objects) {
                case (Obj(ad1, cd1, c1), Obj(ad2, cd2, c2)) =>
                    val (abstractDesc, concreteDesc) = if (c1 < c2) {
                        val abstractified = abstractifyConcreteDesc(cd1, accessor)
                        val newAd1 = if (wasAbstractified(c1)) mergeAbsDesc(abstractified, ad1) else abstractified
                        mergeAbsDesc(newAd1, ad2) -> cd2
                    } else if (c1 > c2) {
                        val abstractified = abstractifyConcreteDesc(cd2, accessor)
                        val newAd2 = if (wasAbstractified(c2)) mergeAbsDesc(abstractified, ad2) else abstractified
                        mergeAbsDesc(ad1, newAd2) -> cd1
                    } else {
                        mergeAbsDesc(ad1, ad2) -> mergeConcreteDesc(cd1, cd2)
                    }

                    Obj(abstractDesc, concreteDesc, Math.max(c1, c2))
            }

            val newBoxedValues = Utils.mergeMaps(boxedValues, other.boxedValues) { _ unify _}

            val changed = newBoxedValues != boxedValues || newObjects != objects

            objects = newObjects
            boxedValues = newBoxedValues

            changed
        }

        override def accessor: SimpleMutator = new SimpleMutator(objects, boxedValues, shared)
    }



    private class SimpleMutator(var objects: Map[Location, Obj] = Map.empty, var boxedValues: Map[Location, BoxedValue], val shared: Shared) extends HeapMutatorImplementation {
        override def config: Heap.Config = shared.config

        override def specialObject(specialObject: SpecialObject): ObjectLike = shared.specialObjects(specialObject)

        override def getObject(loc: Location): Option[Obj] = objects.get(loc)
        override def getBox(loc: Location): Option[BoxedValue] = boxedValues.get(loc)

        override def setObject(loc: Location, obj: Obj): Unit = objects += loc -> obj
        override def setBox(loc: Location, box: BoxedValue): Unit = boxedValues += loc -> box

    }
}