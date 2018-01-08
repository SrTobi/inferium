package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap.{IniEntity, IniObject}

import scala.collection.mutable


//abstract class HeapHandle

abstract class HeapMemory {
    //def read(handle: HeapHandle): ValueLike
    //def write(handle: HeapHandle, value: ValueLike): Unit
    def readProperty(target: ValueLike, propertyName: String, cache: Boolean = true): ValueLike
    def writeProperty(target: ValueLike, propertyName: String, handle: ValueLike): Unit
    def listProperties(target: ValueLike): collection.Set[String]

    def filterUnwritables(target: ValueLike): ValueLike
    def manipulateReference(ref: Reference, value: ValueLike): Unit
    def manipulateIfReference(ref: ValueLike, value: ValueLike): Unit

    def split(): HeapMemory
    def squashed(): HeapMemory

    def toIniEntity(objects: Seq[ValueLike]): Seq[(ValueLike, IniEntity)] = {
        val foundObjects = mutable.Map.empty[ValueLike, IniObject]
        return objects.map(obj => (obj, toIniEntityRec(obj, foundObjects)))
    }

    private def toIniEntityRec(obj: ValueLike, objects: mutable.Map[ValueLike, IniObject]): IniEntity = {
        objects.get(obj) match {
            case Some(value) =>
                return value
            case _ =>
        }

        obj.normalized match {
            case UnionValue(values) => IniEntity(values.map(toIniEntityRec(_, objects)): _*)
            case obj: ObjectValue =>
                val properties = mutable.Map.empty[String, IniEntity]
                val iniObj = new IniObject(properties)(Some(obj.internalId))
                objects.update(obj, iniObj)

                for (prop <-listProperties(obj).iterator) {
                    val value = readProperty(obj, prop, cache = false)
                    properties += (prop -> toIniEntityRec(value, objects))
                }
                iniObj
            case value => IniEntity(value)
        }
    }
}

abstract class Heap {
    def newEmptyHeapState(): HeapMemory

    def unify(heaps: HeapMemory*): HeapMemory
}

object Heap {
    abstract class IniEntity {
        def members: collection.Map[String, IniEntity]
    }
    class IniObject(override val members: collection.Map[String, IniEntity])(val id: Option[Long] = None) extends IniEntity {
        override def equals(o: scala.Any): Boolean = o match {
            case other: IniObject =>
                if (id.isDefined && id == other.id) {
                    return true
                }
                return members == other.members
            case _ =>
                false
        }

        override def hashCode(): Int = members.hashCode()
        override def toString: String = s"#${id.getOrElse("")}{${members.take(5).map{ case (prop, value) => s"$prop -> $value"}.mkString(", ")}}"
    }
    case class IniValue(value: Value) extends IniEntity {
        override lazy val members: collection.Map[String, IniEntity] = Map()
    }
    case class IniUnion(values: collection.Set[IniEntity]) extends IniEntity {
        assert(values.size >= 2)

        override lazy val members: collection.Map[String, IniEntity] = ???

        override def toString: String = values.mkString("[", " | ", "]")
    }

    object IniEntity {
        def apply(something: Any*): IniEntity = something match {
            case Seq() => IniValue(NeverValue)
            case Seq(value: IniEntity) => value
            case Seq(value) => IniValue(Value(value))
            case all =>
                val set = all.map(IniEntity(_)).toSet
                if (set.size == 1) set.head else IniUnion(set)
        }
    }

    object IniObject {
        private def toPropertyValue(any: Any): IniEntity = any match {
            case obj: IniObject => obj
            case anything => IniValue(Value(anything))
        }
        def apply(valueMembers: (String, Any)*): IniObject = new IniObject(Map(valueMembers.map { case (prop, value) => (prop, toPropertyValue(value))}: _*))()
    }

    type PropertyValue = Either[IniObject, ValueLike]

    def writeIniObjectToHeap(heap: HeapMemory, obj: IniObject, objMap: mutable.Map[IniObject, ObjectValue] = mutable.Map.empty[IniObject, ObjectValue]): ObjectValue = {
        return objMap.getOrElse(obj, {
            val objValue = new ObjectValue
            objMap += (obj -> objValue)
            obj.members foreach {
                case (prop, member) =>
                    val value = member match {
                        case memberObj: IniObject=> writeIniObjectToHeap(heap, memberObj, objMap)
                        case IniValue(valueMember) => valueMember
                    }
                    heap.writeProperty(objValue, prop, value)
            }
            objValue
        })
    }
}



/*
import Nodes.Node

class HeapStateBuilder(private var cur: HeapState.State, private val node: Node) {
    def newHandleReader(handle: HeapState.ValueHandle, handler: HeapState.ValueHandleChangeHandler): HeapState.HandleReader = {
        assert(isBuilding)
        val (reader, newHeap) = cur.newHandleReader(handle, handler)
        cur = newHeap
        return reader
    }

    def newHandleWriter(handle: HeapState.ValueHandle): HeapState.HandleWriter = {
        assert(isBuilding)
        val (writer, newHeap) = cur.newHandleWriter(handle)
        cur = newHeap
        return writer
    }

    def newHandleMerge(): HeapState.ValueHandleMerger = {
        assert(isBuilding)
        val (merger, newHeap) = cur.newValueHandleMerger()
        cur = newHeap
        return merger
    }

    /*def readProperty(target: HeapState.ValueHandle, property: String, into: HeapState.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.readProperty(target, property, into)
    }

    def writeProperty(target: HeapState.ValueHandle, property: String, value: HeapState.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.writeProperty(target, property, value)
    }

    def readLocal(target: EmptyObject, property: String, into: HeapState.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.readLocal(target, property, into)
    }

    def writeLocal(target: EmptyObject, property: String, value: HeapState.ValueHandle): Unit = {
        assert(isBuilding)
        cur = cur.writeLocal(target, property, value)
    }*/

    def end(): HeapState.State = {
        if (cur == null) {
            throw new IllegalStateException("Can not end a builder a second time")
        }
        val result = cur
        cur = null
        return result
    }

    def isBuilding: Boolean = !hasEnded
    def hasEnded: Boolean = cur == null
}

trait HeapState {
    def newEmptyHeapState(): HeapState.State
    def newMergeHeapState(numTip: Int = 0): HeapState.MergeState
    def newValueHandle(): HeapState.ValueHandle

    def propagateFlow(startHeap: HeapState.State): Boolean
}

object HeapState {
    trait ValueHandle

    trait ValueHandleChangeHandler extends ValueChangeHandler {
        def onHandleChanged(oldValue: Option[Value], newValue: Value): Unit
        override def onValueChanged(): Unit = {}
    }

    trait ValueHandleMerger extends ValueHandle {
        def add(handles: ValueHandle*): Unit
    }

    trait HandleReader {
        def read(): Value
    }

    trait HandleWriter {
        def write(value: Value): Unit
    }

    trait State {
        def newHandleReader(handle: ValueHandle, handler: ValueHandleChangeHandler): (HandleReader, State)
        def newHandleWriter(handle: ValueHandle): (HandleWriter, State)
        def newValueHandleMerger(): (HeapState.ValueHandleMerger, State)

        def truthyfy(cond: ValueHandle): State
        def falsyfy(cond: ValueHandle): State
    }

    trait MergeState extends State {
        def addInflow(heapState: State): Unit
    }
}*/