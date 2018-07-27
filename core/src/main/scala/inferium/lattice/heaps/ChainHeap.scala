package inferium.lattice.heaps

import java.util

import inferium.Unifiable
import inferium.lattice.Heap.SpecialObjects.SpecialObject
import inferium.lattice.Heap.{Mutator, Shared, SpecialObjectMap}
import inferium.lattice._
import inferium.utils.Utils

import scala.collection.mutable



object ChainHeap extends Heap.Factory with HeapImmutables {
    override def create(config: Heap.Config): (Heap, SpecialObjectMap) = {
        val shared = new Shared(config, mutable.Map.empty)
        val heap = new HeapElement(shared, new HeapRoot(shared))
        (heap, shared.specialObjects)
    }

    private class Shared(config: Heap.Config, specialObjects: SpecialObjectMap) extends Heap.Shared(config, specialObjects)

    private trait HeapProvider {
        def getObject(loc: Location): Option[Obj]
        def getBox(loc: Location): Option[BoxedValue]
    }

    private sealed trait HeapStage extends HeapProvider {
        def shared: Shared
        def depth: Int
        def parent: HeapStage
        def anchor: AnyRef
    }

    private class HeapRoot(override val shared: Shared) extends HeapStage {
        override def depth: Int = 0
        override def parent: HeapStage = this

        override def getObject(loc: Location): Option[Obj] = None
        override def getBox(loc: Location): Option[BoxedValue] = None

        override def anchor: AnyRef = this
    }

    private class HeapElement(override val shared: Shared,
                              val parent: HeapStage,
                              val objects: Map[Location, Obj] = Map.empty,
                              val boxedValues: Map[Location, BoxedValue] = Map.empty) extends Heap with HeapStage {

        override val depth: Int = parent.depth + 1

        private def createMutator(): HeapStageMutator = {
            new HeapStageMutator(this, objects, boxedValues)
        }
        override def begin(location: Location): HeapStageMutator = createMutator()

        override def end(actor: Heap.Mutator): Heap = {
            val mutator = actor.asInstanceOf[HeapStageMutator]
            assert(mutator.origin eq this)
            if ((mutator.objects eq objects) && (mutator.boxedValues eq boxedValues))
                this
            else
                new HeapElement(shared, parent, mutator.objects, mutator.boxedValues)
        }

        override def unify(heaps: Seq[Heap])(implicit fixpoint: Unifiable.Fixpoint): Heap = {
            val allHeaps = (this +: heaps) map { _.asInstanceOf[HeapElement] }
            /*val mutator = createMutator()
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

            new HeapElement(shared, newObjects, newBoxedValues)*/

            val unifiers = mutable.Map.empty[AnyRef, Unifier]
            val queue = mutable.PriorityQueue.empty[Unifier](Ordering.by(_.depth))

            def addToUnifiers(unifier: Unifier): Unit = {
                unifiers.get(unifier.parent.anchor) match {
                    case None =>
                        unifiers += unifier.parent.anchor -> unifier
                        queue += unifier
                    case Some(equalUnifier) =>
                        equalUnifier.merge(unifier)
                }
            }

            allHeaps.map(new Unifier(_)).foreach(addToUnifiers)

            while(true) {
                val unifier = queue.dequeue()

                if (queue.isEmpty) {
                    return unifier.toHeapElement(shared)
                }

                unifiers.remove(unifier.parent.anchor)
                unifier.up()
                addToUnifiers(unifier)
            }

            throw new IllegalStateException("should not be reached")
        }

        override def equals(other: scala.Any): Boolean = other match {
            case other: HeapElement =>
                (other eq this) ||
                    ((parent eq other.parent) && objects == other.objects && boxedValues == other.boxedValues)

            case _ =>
                false
        }

        override def createGlobalHeap(): GlobalHeap = {
            val unifier = new Unifier(this)
            while (unifier.canUp) {
                unifier.up()
            }
            val self = unifier.toHeapElement(shared)
            val acc = self.createMutator()
            new ChainGlobalHeap(shared, objects map { case (k, v) => k -> abstractifyObject(v, acc, null)  })
        }

        override def getObject(loc: Location): Option[Obj] = objects.get(loc).orElse(parent.getObject(loc))
        override def getBox(loc: Location): Option[BoxedValue] = boxedValues.get(loc).orElse(parent.getBox(loc))

        override val anchor: AnyRef = new AnyRef
    }

    private class Unifier(iniHeap: HeapElement) {
        private val objects =  mutable.Map.empty[Location, Obj]
        private val boxedValues =  mutable.Map.empty[Location, BoxedValue]

        objects ++= iniHeap.objects
        boxedValues ++= iniHeap.boxedValues

        var depth: Int = iniHeap.depth
        var parent: HeapStage = iniHeap.parent
        var current: HeapElement = iniHeap

        private def getObject(loc: Location) = objects.get(loc).orElse(parent.getObject(loc))
        private def getBox(loc: Location) = boxedValues.get(loc).orElse(parent.getBox(loc))

        def canUp: Boolean = parent.isInstanceOf[HeapElement]

        def up(): Unit = {
            assert(canUp)
            for (entry@(loc, _) <- objects if !objects.contains(loc)) {
                objects += entry
            }

            for (entry@(loc, _) <- boxedValues if !boxedValues.contains(loc)) {
                boxedValues += entry
            }

            depth = parent.depth
            current = parent.asInstanceOf[HeapElement]
            parent = current
        }

        def merge(other: Unifier): Unit = {
            val mutatorForOther = new HeapStageMutator(parent, other.current.objects, other.current.boxedValues)
            val mutatorForMe = new HeapStageMutator(parent, current.objects, current.boxedValues)

            for (entry@(loc, obj1@Obj(ad1, cd1, c1)) <- other.objects) {
                getObject(loc) match {
                    case Some(obj2) if obj2 eq obj1 =>
                        objects += loc -> obj1
                    case Some(obj2@Obj(ad2, cd2, c2)) =>
                        val (abstractDesc, concreteDesc) = if (c1 < c2) {
                            val abstractified = abstractifyConcreteDesc(cd1, mutatorForOther)
                            val newAd1 = if (wasAbstractified(c1)) mergeAbsDesc(abstractified, ad1) else abstractified
                            mergeAbsDesc(newAd1, ad2) -> cd2
                        } else if (c1 > c2) {
                            val abstractified = abstractifyConcreteDesc(cd2, mutatorForMe)
                            val newAd2 = if (wasAbstractified(c2)) mergeAbsDesc(abstractified, ad2) else abstractified
                            mergeAbsDesc(ad1, newAd2) -> cd1
                        } else {
                            mergeAbsDesc(ad1, ad2) -> mergeConcreteDesc(cd1, cd2)
                        }

                        if (c1 != c2 || (abstractDesc ne ad2) || (concreteDesc ne cd2))
                            objects += loc -> Obj(abstractDesc, concreteDesc, Math.max(c1, c2))
                    case None =>
                        objects += loc -> obj1
                }
            }

            for ((loc, otherBox) <- other.boxedValues) {
                getBox(loc) match {
                    case Some(myBox) =>
                        if (myBox ne otherBox) {
                            boxedValues += loc -> (myBox unify otherBox)
                        }
                    case None =>
                        boxedValues += loc -> otherBox
                }
            }
        }

        def toHeapElement(shared: Shared): HeapElement = new HeapElement(shared, parent, objects.toMap, boxedValues.toMap)
    }


    private class GlobalHeapLink(override val shared: Shared,
                                 val globalHeap: ChainGlobalHeap,
                                 objects: Map[Location, Obj] = Map.empty) extends HeapStage {

        val objectReads = mutable.Map.empty[Location, Obj]

        override def getObject(loc: Location): Option[Obj] = {
            val objOpt = objects.get(loc)
            objectReads += loc -> objOpt.orNull
            objOpt
        }
        override def getBox(loc: Location): Option[ChainHeap.BoxedValue] = None

        override def depth: Int = 0
        override def parent: HeapStage = this

        override def anchor: AnyRef = globalHeap
    }

    private class ChainGlobalHeap(val shared: Shared, var objects: Map[Location, Obj]) extends GlobalHeap with HeapStage {

        private val links = mutable.Map.empty[Location, GlobalHeapLink]

        override def hasEffect(location: Location): Boolean = {
            for(link <- links.get(location)) {
                for ((objLoc, obj) <- link.objectReads) {
                    objects.get(objLoc) match {
                        case Some(obj2) =>
                            if (obj != obj2)
                                return true
                        case None =>
                            return true
                    }
                }

                return false
            }
            return true
        }

        override def toHeap(location: Location): Heap = {
            val link = new GlobalHeapLink(shared, this, objects )
            links += location -> link
            new HeapElement(shared, link)
        }

        override def feed(heap: Heap): Boolean = {
            val unifier = new Unifier(heap.asInstanceOf[HeapElement])
            while (unifier.canUp) {
                unifier.up()
            }
            val other = unifier.toHeapElement(shared)

            val acc = heap.begin(Location())
            val newObjects = Utils.mergeMaps(objects, other.objects mapValues { abstractifyObject(_, acc, null) }) {
                case (Obj(ad1, _, abs1), Obj(ad2, _, abs2)) =>
                    Obj(mergeAbsDesc(ad1, ad2), null, Math.max(abs1, abs2))
            }

            val changed = newObjects != objects

            objects = newObjects

            changed
        }

        override def accessor: HeapStageMutator = new HeapStageMutator(this, objects, Map.empty)
        override def depth: Int = 0
        override def parent: HeapStage = this
        override def getObject(loc: Location): Option[ChainHeap.Obj] = objects.get(loc)
        override def getBox(loc: Location): Option[ChainHeap.BoxedValue] = None

        override def anchor: AnyRef = this
    }






    private class HeapStageMutator(val origin: HeapStage,
                                   var objects: Map[Location, Obj],
                                   var boxedValues: Map[Location, BoxedValue]) extends HeapMutatorImplementation {
        private def shared = origin.shared
        override def config: Heap.Config = shared.config
        override def specialObject(specialObject: SpecialObject): ObjectLike = shared.specialObjects(specialObject)


        def getObject(loc: Location): Option[Obj] = objects.get(loc).orElse(origin.parent.getObject(loc))
        def getBox(loc: Location): Option[BoxedValue] = boxedValues.get(loc).orElse(origin.parent.getBox(loc))

        override def setObject(loc: Location, obj: ChainHeap.Obj): Unit = objects += loc -> obj
        override def setBox(loc: Location, box: ChainHeap.BoxedValue): Unit = boxedValues += loc -> box
    }
}