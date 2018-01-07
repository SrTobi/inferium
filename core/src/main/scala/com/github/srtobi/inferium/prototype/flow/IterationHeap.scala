package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.collection.mutable


class IterationHeap extends Heap {
    import IterationHeap._

    override def newEmptyHeapState(): HeapMemory = new Memory()
    override def unify(heaps: HeapMemory*): HeapMemory = Memory.unify(heaps.map(_.asInstanceOf[Memory]))
}

object IterationHeap {

    type MemoryMap = mutable.Map[Long, Properties]
    type Properties = (ObjectValue, mutable.Map[String, (Int, ValueLike)])

    class Memory(val idx: Int = 0, val prev: Option[Memory] = None, private val objects: MemoryMap = mutable.Map.empty) extends HeapMemory {

        private var ended = false
        private var writeCount = 0

        private def nextWriteId(): Int = {
            writeCount += 1
            return writeCount
        }

        private def set(obj: ObjectValue, property: String, value: ValueLike, writeId: Int): Unit = {
            assert(!ended)
            val id = obj.internalId
            val (storedObj, properties) = objects.getOrElseUpdate(id, (obj, mutable.Map.empty))
            properties += (property -> (writeId, value))
            /*if (storedObj != obj) {
                objects.update(id, (obj, properties))
            }*/
        }

        private[this] def getHere(obj: ObjectValue, property: String): Option[(Int, ValueLike)] = {
            return objects.get(obj.internalId).flatMap(_._2.get(property))
        }

        private def get(obj: ObjectValue, baseObjects: Set[ObjectValue], property: String, cache: Boolean): ValueLike = {
            /*val entry = objects.get(obj.internalId) match {
                case Some((storedObj, properties)) =>
                    if (storedObj != obj) {
                        objects.update(obj.internalId, (obj, properties))
                    }
                    properties.get(property)
                case _ => None
            }*/
            getHere(obj, property) match {
                case Some((id, value)) =>
                    val overwritingProps = baseObjects.toSeq.flatMap(getHere(_, property)).filter(_._1 > id).map(_._2)
                    if (baseObjects.nonEmpty && overwritingProps.size == baseObjects.size) {
                        UnionValue(overwritingProps: _*)
                    } else {
                        UnionValue(value +: overwritingProps: _*)
                    }
                case None =>
                    val baseProps = baseObjects.flatMap(base => getHere(base, property).map{ case (_, v) => (v, base)})
                    val restBaseObjects = baseObjects -- baseProps.map(_._2)
                    val valuesFromBase = baseProps.map(_._1).toSeq
                    val result = if (restBaseObjects.isEmpty && baseObjects.nonEmpty) {
                        UnionValue(valuesFromBase: _*)
                    } else {
                        val recResult = prev.map(_.get(obj, restBaseObjects, property, cache = false)).getOrElse(UndefinedValue)
                        UnionValue(recResult +: valuesFromBase: _*)
                    }
                    if (cache)
                        set(obj, property, result, -1)
                    result
            }
        }

        override def readProperty(target: ValueLike, property: String, cache: Boolean): ValueLike = {
            val result = target.asObject.map(get(_, target.baseObjects.toSet, property, cache = cache)).getOrElse(UndefinedValue)
            if (target.propertyWriteMaybeNoOp) UnionValue.withUndefined(result) else result
        }

        override def writeProperty(target: ValueLike, propertyName: String, value: ValueLike): Unit = {
            val writeId = nextWriteId()
            target.asObject.foreach {
                set(_, propertyName, value, writeId)
            }

            val baseObjects = target.baseObjects.toSeq
            val mergeWithPrevious = baseObjects.length > 1
            baseObjects foreach {
                obj =>
                    val mergedValue = if (mergeWithPrevious) UnionValue(readProperty(obj, propertyName, cache = false), value) else value
                    set(obj, propertyName, mergedValue, writeId)
            }
        }

        override def filterUnwritables(target: ValueLike): ValueLike = {
            return target.remove(this, _.throwsWhenWrittenOrReadOn)
        }

        override def manipulateReference(ref: Reference, newValue: ValueLike): Unit = ref match {
            case Reference(value, obj, property) =>
                val org = readProperty(obj, property, cache = true)
                if (org == value) {
                    // we now know that the reference still applies and can now change it
                    // but that might be another reference and we can trie to change that as well
                    writeProperty(obj, property, newValue)
                }
        }

        override def manipulateIfReference(value: ValueLike, newValue: ValueLike): Unit = value match {
            case ref: Reference =>
                manipulateReference(ref, newValue)
            case _ =>
        }

        override def split(): HeapMemory = {
            ended = true
            new Memory(idx + 1, if (objects.isEmpty) prev else Some(this))
        }

        override def toString: String = s"Heap[$idx]"
    }

    object Memory {

        private class Unifier(iniMemory: Memory) {
            var memory: Memory = iniMemory.prev.get
            def index: Int = memory.idx
            val objects: MemoryMap = iniMemory.objects.map { case (obj, (prop, value)) => (obj, (prop, value.clone()))}

            def up(): Unit = {
                assert(memory.prev.nonEmpty)
                for (pair@(obj, _) <- memory.objects) {
                    if (!objects.contains(obj)) {
                        objects += pair
                    }
                }
                memory = memory.prev.get
            }

            def merge(other: Unifier): Unit = {
                val objSet = objects.keySet ++ other.objects.keySet

                objSet.foreach {
                    id =>
                        val (bObj, bProps): Properties = other.objects.getOrElse(id, (null, mutable.Map()))
                        val (aObj, aProps) = objects.getOrElseUpdate(id, (bObj, mutable.Map()))
                        val props = aProps.keySet ++ bProps.keySet
                        props.foreach {
                            prop =>
                                lazy val default = memory.readProperty(aObj, prop, cache = false)
                                val aVal = aProps.get(prop).map(_._2).getOrElse(default)
                                val bVal = bProps.get(prop).map(_._2).getOrElse(default)
                                aProps.update(prop, (0, UnionValue(aVal, bVal)))
                        }
                }
            }
        }

        def unify(memories: Seq[Memory]): Memory = {
            assert(memories.nonEmpty)
            assert(memories.distinct.length == memories.length)

            memories match {
                case Seq(mem) =>
                    return mem
                case _ =>
            }

            memories.foreach(_.ended = true)
            val newIndex = memories.map(_.idx).max + 1
            val unifiers = mutable.Map.empty[Memory, Unifier]
            // the greatest index must be front!
            val queue = mutable.PriorityQueue.empty[Unifier](Ordering.by(_.index))

            def addToUnifiers(unifier: Unifier): Unit = {
                unifiers.get(unifier.memory) match {
                    case Some(equalUnifier) =>
                        equalUnifier.merge(unifier)
                    case None =>
                        unifiers += (unifier.memory -> unifier)
                        queue += unifier
                }
            }

            memories.map(new Unifier(_)).foreach(addToUnifiers)

            while(true) {
                val unifier = queue.dequeue()

                if (queue.isEmpty) {
                    return new Memory(newIndex, Some(unifier.memory), unifier.objects)
                }

                unifiers -= unifier.memory
                unifier.up()
                addToUnifiers(unifier)
            }

            throw new IllegalStateException("should not be reached")
        }
    }
}
