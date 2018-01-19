package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap.{IniEntity, IniFunctionInfo, IniObject, UserObjectInfo}

import scala.collection.mutable


//abstract class HeapHandle

abstract class HeapMemory {
    //def read(handle: HeapHandle): ValueLike
    //def write(handle: HeapHandle, value: ValueLike): Unit
    def createObject(target: ObjectValue): Unit
    def readProperty(target: ValueLike, propertyName: String, cache: Boolean = true): ValueLike
    def writeProperty(target: ValueLike, propertyName: String, handle: ValueLike): Unit
    def listProperties(target: ValueLike): collection.Set[String]

    def filterUnwritables(target: ValueLike): ValueLike
    def manipulateReference(ref: Reference, value: ValueLike): Unit
    def manipulateIfReference(ref: ValueLike, value: ValueLike): Unit

    def split(): HeapMemory
    def squashed(): HeapMemory

    def structureEquals(o: HeapMemory): Boolean

    def toIniEntity(objects: Seq[ValueLike], analysis: FlowAnalysis): Seq[(ValueLike, IniEntity)] = {
        val foundObjects = mutable.Map.empty[ValueLike, IniObject]
        return objects.map(obj => (obj, toIniEntityRec(obj, foundObjects, analysis)))
    }

    private def toIniEntityRec(obj: ValueLike, objects: mutable.Map[ValueLike, IniObject], analysis: FlowAnalysis): IniEntity = {
        objects.get(obj) match {
            case Some(value) =>
                return value
            case _ =>
        }
        def toIni(obj: ValueLike) = toIniEntityRec(obj, objects, analysis)

        obj.normalized match {
            case UnionValue(values) => IniEntity(values.map(toIniEntityRec(_, objects, analysis)): _*)
            case obj: ObjectValue =>
                val properties = mutable.Map.empty[String, IniEntity]
                val iniObj = new IniObject(properties)(Some(obj.internalId))
                objects.update(obj, iniObj)

                val functionInfo = obj match {
                    case uv: UserValue =>
                        val userInfo = UserObjectInfo(uv.properties.mapValues(toIni), Map())
                        iniObj.userInfo = Some(userInfo)
                        uv.functionInfo.map(info => IniFunctionInfo(toIni(info.returnValue), info.parameter.map(toIni)))
                    case f: FunctionValue =>
                        val info = analysis.getFunctionInfo(f).get
                        Some(IniFunctionInfo(toIni(info.returnValue), info.arguments.map(toIni)))
                    case _ =>
                        None
                }
                iniObj.functionInfo = functionInfo

                for (prop <-listProperties(obj).iterator) {
                    val value = readProperty(obj, prop, cache = false)
                    properties += (prop -> toIni(value))
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

        def hashCodeRec(deep: Int): Int = hashCode()
    }
    case class IniFunctionInfo(returnValue: IniEntity, parameter: Seq[IniEntity]) {
        override def toString: String = parameter.mkString("(", ", ", ") => " + returnValue)
    }

    case class UserObjectInfo(readProperties: collection.Map[String, IniEntity], writtenProperties: collection.Map[String, IniEntity])

    class IniObject(override val members: collection.Map[String, IniEntity])(val id: Option[Long] = None) extends IniEntity {
        override def equals(o: scala.Any): Boolean = o match {
            case other: IniObject =>
                if (id.isDefined && other.id.isDefined) {
                    return id == other.id
                }
                return members == other.members
            case _ =>
                false
        }

        var functionInfo: Option[IniFunctionInfo] = None
        var userInfo: Option[UserObjectInfo] = None

        def isUserObject: Boolean = userInfo.isDefined

        private lazy val _hashCode = hashCodeRec(0)
        override def hashCode(): Int = _hashCode
        override def hashCodeRec(deep: Int): Int = if (deep > 2) 0 else members.keySet.hashCode() ^ members.values.foldLeft(0){ case (acc, mem) => acc ^ mem.hashCodeRec(deep + 1) }
        override def toString: String = s"#${id.getOrElse("")}{${members.take(5).map{ case (prop, value) => s"$prop -> $value"}.mkString(", ")}}"
    }
    case class IniValue(value: Value) extends IniEntity {
        override lazy val members: collection.Map[String, IniEntity] = Map()

        override def toString: String = value.toString
    }
    case class IniUnion(values: collection.Set[IniEntity]) extends IniEntity {
        assert(values.size >= 2)

        override lazy val members: collection.Map[String, IniEntity] = ???

        override def toString: String = values.mkString("[", " | ", "]")

        private lazy val _hashCode = hashCodeRec(0)
        override def hashCode(): Int = _hashCode
        override def hashCodeRec(deep: Int): Int = if (deep > 2) 0 else values.foldLeft(0){ case (acc, mem) => acc ^ mem.hashCodeRec(deep + 1) }
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