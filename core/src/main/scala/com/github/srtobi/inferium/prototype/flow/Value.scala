package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.lattice.BoolLattice

import scala.collection.mutable

abstract class ValueLike {
    def asValue: Value
    def normalized: Value
    def asBool: BoolLattice
    def asFunctions: Traversable[FunctionValue]
    def asObject: Option[ObjectValue]
    def baseObjects: Traversable[ObjectValue]
    def propertyWriteMaybeNoOp: Boolean

    def without(filter: (ValueLike) => Boolean): ValueLike
    def remove(heap: HeapMemory, filter: (ValueLike) => Boolean): ValueLike

    def truthy(heap: HeapMemory): ValueLike
    def falsy(heap: HeapMemory): ValueLike

    def throwsWhenWrittenOrReadOn: Boolean

    def asReference: Option[Reference]
}

sealed abstract class Value extends ValueLike {

    override def asValue: Value = this
    override def normalized: Value = asValue
    override def asBool: BoolLattice = BoolLattice.True
    override def asFunctions: Traversable[FunctionValue] = Seq.empty
    override def asObject: Option[ObjectValue] = None
    override def propertyWriteMaybeNoOp: Boolean = true
    override def baseObjects: Traversable[ObjectValue] = Traversable()

    override def without(filter: ValueLike => Boolean): ValueLike = if (filter(this)) NeverValue else this
    override def remove(heap: HeapMemory, filter: (ValueLike) => Boolean): ValueLike = without(filter)

    override def throwsWhenWrittenOrReadOn: Boolean = false

    def asReference: Option[Reference] = None

    /*def in: scala.collection.Set[Value] = inValues
    def out: scala.collection.Set[Value] = outValues

    final def flowsTo(toValue: Value): Unit = {
        if (outValues.add(toValue)) {
            toValue.inValues += this

            // notify
            this.onOutFlow(toValue)
            toValue.onInFlow(this)
        }
    }

    final def removeFlowTo(toValue: Value): Unit = {
        if (outValues.remove(toValue)) {
            toValue.inValues -= this

            // notify
            this.onOutFlowRemoved(toValue)
            toValue.onInFlowRemoved(this)
        }
    }

    def onInFlow(inValue: Value): Unit = {}
    def onOutFlow(outValue: Value): Unit = {}
    def onInFlowRemoved(inValue: Value): Unit = {}
    def onOutFlowRemoved(outValue: Value): Unit = {}*/

    //def getProperty(name: String): scala.collection.Set[HeapState.ValueHandle]
}

object Value {
    def apply(any: Any): Value = any match {
        case value: Value => value
        case number: Int => SpecificNumberValue(number)
        case string: String => SpecificStringValue(string)
        case bool: Boolean => SpecificBoolValue(bool)
        case null => NullValue
        case _ => throw new IllegalArgumentException(s"Can not convert $any to a property value!")
    }
}

object NeverValue extends Value {
    override def throwsWhenWrittenOrReadOn: Boolean = true
    override def asBool: BoolLattice = BoolLattice.Top

    override def truthy(heap: HeapMemory): ValueLike = NeverValue
    override def falsy(heap: HeapMemory): ValueLike = NeverValue

    override def toString: String = "never"
}

object UndefinedValue extends Value {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true

    override def truthy(heap: HeapMemory): ValueLike = NeverValue
    override def falsy(heap: HeapMemory): ValueLike = this

    override def toString: String = "undefined"
}

object NullValue extends Value {
    override def asBool: BoolLattice = BoolLattice.False
    override def throwsWhenWrittenOrReadOn: Boolean = true

    override def truthy(heap: HeapMemory): ValueLike = NeverValue
    override def falsy(heap: HeapMemory): ValueLike = this

    override def toString: String = "null"
}

sealed abstract class BoolValue extends Value {
    override def without(filter: ValueLike => Boolean): ValueLike = {
        val trueValue = !filter(TrueValue)
        val falseValue = !filter(TrueValue)
        return BoolValue(BoolLattice(trueValue).unify(BoolLattice(falseValue)))
    }

    override def truthy(heap: HeapMemory): ValueLike = TrueValue
    override def falsy(heap: HeapMemory): ValueLike = FalseValue
}
object BoolValue extends BoolValue {
    def apply(value: BoolLattice): BoolValue = value match {
        case BoolLattice.Top => BoolValue
        case BoolLattice.False => FalseValue
        case BoolLattice.True => TrueValue
    }

    override def asBool: BoolLattice = BoolLattice.Top

    override def toString: String = "boolean"
}

sealed abstract class SpecificBoolValue protected(val value: Boolean) extends BoolValue {
    override def toString: String = value.toString
}

object TrueValue extends SpecificBoolValue(true) {
    override def asBool: BoolLattice = BoolLattice.True

}

object FalseValue extends SpecificBoolValue(false) {
    override def asBool: BoolLattice = BoolLattice.False

    override def toString: String = "false"
}

object SpecificBoolValue {
    def apply(value: Boolean): SpecificBoolValue = if (value) TrueValue else FalseValue

    def unapply(arg: SpecificBoolValue): Option[Boolean] = Some(arg.value)
}

sealed abstract class NumberValue extends Value
object NumberValue extends NumberValue {
    override def asBool: BoolLattice = BoolLattice.Top

    override def truthy(heap: HeapMemory): ValueLike = this
    override def falsy(heap: HeapMemory): ValueLike = SpecificNumberValue(0)

    override def toString: String = "number"
}

case class SpecificNumberValue(value: Int) extends NumberValue {
    override def asBool: BoolLattice = BoolLattice(value != 0)

    override def truthy(heap: HeapMemory): ValueLike = if (value == 0) NeverValue else this
    override def falsy(heap: HeapMemory): ValueLike = if (value == 0) this else NeverValue

    override def toString: String = value.toString
}

sealed abstract class StringValue extends Value
object StringValue extends StringValue {
    override def asBool: BoolLattice = BoolLattice.Top

    override def truthy(heap: HeapMemory): ValueLike = this
    override def falsy(heap: HeapMemory): ValueLike = SpecificStringValue("")

    override def toString: String = "string"
}

case class SpecificStringValue(value: String) extends StringValue {
    override def asBool: BoolLattice = BoolLattice(value != "")

    override def truthy(heap: HeapMemory): ValueLike = if (value == "") NeverValue else this
    override def falsy(heap: HeapMemory): ValueLike = if (value == "") this else NeverValue

    override def toString: String = "\"" + value + "\""
}

sealed case class ObjectValue(internalId: Long = ObjectValue.nextObjId()) extends Value {
    override def asObject: Option[ObjectValue] = Some(this)
    override def propertyWriteMaybeNoOp: Boolean = false

    override def truthy(heap: HeapMemory): ValueLike = this
    override def falsy(heap: HeapMemory): ValueLike = NeverValue

    override def toString: String = s"obj#$internalId"
}

object ObjectValue {
    private var nextId: Long = 0
    def nextObjId(): Long = {
        nextId += 1
        return nextId
    }
}

final class FunctionValue(val template: Templates.Function, val closures: Seq[ValueLike]) extends ObjectValue() {
    override def asFunctions: Traversable[FunctionValue] = Traversable(this)

    override def toString: String = s"func#$internalId"
}

object FunctionValue {
    def unapply(fv: FunctionValue): Option[(Templates.Function, Seq[ValueLike])] = Some((fv.template, fv.closures))
}

final class UnionValue private (id: Long, val values: Seq[ValueLike]) extends ObjectValue(id) {

    //assert(values.size >= 2)
    assert(values.forall(v => !v.isInstanceOf[UnionValue]))

    override lazy val asBool: BoolLattice = asBoolImpl()
    private def asBoolImpl(): BoolLattice = {
        val it = values.iterator
        var result: BoolLattice = it.next().asBool
        for (bool <- it) {
            result = result.unify(bool.asBool)
            if (result == BoolLattice.Top) {
                return BoolLattice.Top
            }
        }
        return result
    }

    override def asObject: Option[ObjectValue] = Some(this)
    override def normalized: Value = values match { case Seq(value) => value.normalized; case _ => UnionValue.makeUnion(Some(internalId), values.map(_.normalized)).asValue}
    override def baseObjects: Traversable[ObjectValue] = values.flatMap(_.asObject)
    override def propertyWriteMaybeNoOp: Boolean = values.exists(_.propertyWriteMaybeNoOp)
    override def asFunctions: Traversable[FunctionValue] = values.flatMap(_.asFunctions)
    override def throwsWhenWrittenOrReadOn: Boolean = values.forall(_.throwsWhenWrittenOrReadOn)

    override def without(filter: ValueLike => Boolean): ValueLike = {
        val newSeq = values.filter(!filter(_))
        return if (newSeq.length == values.length) this else UnionValue.unionFromSeq(Some(internalId), newSeq)
    }

    override def remove(heap: HeapMemory, filter: ValueLike => Boolean): ValueLike = {
        if (values.length == 1) {
            return UnionValue.makeUnion(Some(internalId), values.map(_.remove(heap, filter)))
        } else {
            return UnionValue.makeUnion(Some(internalId), values.map(_.without(filter)))
        }
    }

    override def truthy(heap: HeapMemory): ValueLike = UnionValue.makeUnion(Some(internalId), values.map(_.truthy(heap)))
    override def falsy(heap: HeapMemory): ValueLike = UnionValue.makeUnion(Some(internalId), values.map(_.falsy(heap)))

    override def toString: String = s"#$internalId[${values.mkString(" | ")}]"
}

object UnionValue {
    private def unionFromSeq(id: Option[Long], values: Seq[ValueLike]): ValueLike = {
        return values match {
            case Seq() => NeverValue
            case Seq(value) => id.map(new UnionValue(_, values)).getOrElse(value)
            case _ => new UnionValue(id.getOrElse(ObjectValue.nextObjId()), values)
        }
    }

    def apply(values: ValueLike*): ValueLike = {
        return values match {
            case Seq(value) => value
            case Seq(u1:UnionValue, u2:UnionValue) if u1.internalId == u2.internalId => makeUnion(Some(u1.internalId), values)
            case all => makeUnion(None, all)
        }
    }


    private def makeUnion(id: Option[Long], values: Seq[ValueLike]): ValueLike = {
        values match {
            case Seq(value) => id.map(new UnionValue(_, values)).getOrElse(value)
            case _ =>
        }
        var boolValue: BoolValue = null
        var numberValue: NumberValue = null
        var stringValues = mutable.Set.empty[StringValue]
        val objectValues = mutable.Set.empty[ValueLike]
        val conditionalValues = mutable.Map.empty[ConditionalValue, ConditionalValue]
        var hasUndefined = false
        var hasNull = false

        values.flatMap(unpackUnion) foreach {
            case bool: BoolValue =>
                if (boolValue == null || boolValue == bool)
                    boolValue = bool
                else
                    boolValue = BoolValue
            case number: NumberValue =>
                if (numberValue == null || numberValue == number)
                    numberValue = number
                else
                    numberValue = NumberValue
            case StringValue =>
                stringValues = null
            case string: SpecificStringValue =>
                if (stringValues ne null)
                    stringValues.add(string)
            case UndefinedValue =>
                hasUndefined = true
            case NullValue =>
                hasNull = true
            case obj: ObjectValue =>
                objectValues.add(obj)
            case NeverValue =>
            case ref: Reference =>
                val old = conditionalValues.getOrElseUpdate(ref, ref)
                if (ref ne old) {
                    conditionalValues.update(ref, ref.merge(old))
                }

        }

        return unionFromSeq(id,
            (if (hasUndefined) Seq(UndefinedValue) else Seq()) ++
            (if (hasNull) Seq(NullValue) else Seq()) ++
            Option(boolValue).toSeq ++
            Option(numberValue).toSeq ++
            Option(stringValues).getOrElse(Seq(StringValue)) ++
            objectValues ++
            conditionalValues.map{case (_, ref) => ref})
    }

    def withUndefined(value: ValueLike): ValueLike = value match {
        case u@UnionValue(values) => values match {
            case UndefinedValue +: _ => value
            case seq => new UnionValue(u.internalId, UndefinedValue +: seq)
        }
        case UndefinedValue => UndefinedValue
        case NeverValue => UndefinedValue
        case _ => UnionValue(UndefinedValue, value)
    }

    def unapply(arg: UnionValue): Option[Seq[ValueLike]] = Some(arg.values)

    private def unpackUnion(value: ValueLike): Seq[ValueLike] = value match {
        case UnionValue(values) => values
        case NeverValue => Seq()
        case noUnion => Seq(noUnion)
    }
}

case class UnionSet(values: Any*) {
    assert(values.size >= 2)

    override def equals(obj: scala.Any): Boolean = obj match {
        case UnionValue(seq) => seq.map(_.asValue).toSet == values.map(Value(_)).toSet
        case _ => false
    }

    override def toString: String = values.mkString("[", " | ", "]")
}

sealed abstract class ConditionalValue extends ValueLike {
    override def normalized: Value = asValue.normalized
    override def asBool: BoolLattice = asValue.asBool
    override def asFunctions: Traversable[FunctionValue] = asValue.asFunctions
    override def asObject: Option[ObjectValue] = asValue.asObject
    override def baseObjects: Traversable[ObjectValue] = asValue.baseObjects
    override def propertyWriteMaybeNoOp: Boolean = asValue.propertyWriteMaybeNoOp
    override def without(filter: ValueLike => Boolean): ValueLike = asValue.without(filter)
    override def throwsWhenWrittenOrReadOn: Boolean = asValue.throwsWhenWrittenOrReadOn

    def merge(other: ConditionalValue): ConditionalValue
}

final case class Reference(resolved: ValueLike, baseObject: ValueLike, property: String) extends ConditionalValue {
    override val asValue: Value = resolved.asValue

    def asReference: Option[Reference] = Some(this)

    override def remove(heap: HeapMemory, filter: ValueLike => Boolean): Reference = {
        val newValue = resolved.remove(heap, filter)
        heap.manipulateReference(this, newValue)
        return withResolved(newValue)
    }

    override def truthy(heap: HeapMemory): ValueLike = {
        val newBaseObj = baseObject.remove(heap, heap.readProperty(_, property, cache = false).asBool == BoolLattice.False)
        heap.manipulateIfReference(baseObject, newBaseObj)

        val newResolved = resolved.truthy(heap)
        heap.manipulateReference(Reference(resolved, newBaseObj, property), newResolved)

        return Reference(newResolved, newBaseObj, property)
    }

    override def falsy(heap: HeapMemory): ValueLike = {
        val newBaseObj = baseObject.remove(heap, heap.readProperty(_, property, cache = false).asBool == BoolLattice.True)
        heap.manipulateIfReference(baseObject, newBaseObj)

        val newResolved = resolved.falsy(heap)
        heap.manipulateReference(Reference(resolved, newBaseObj, property), newResolved)

        return Reference(newResolved, newBaseObj, property)
    }

    private def withResolved(value: ValueLike) = Reference(value, baseObject, property)

    override def toString: String = s"$resolved{$baseObject.$property}"

    override def merge(otherRaw: ConditionalValue): ConditionalValue = {
        val other = otherRaw.asInstanceOf[Reference]
        return Reference(UnionValue(resolved, other.resolved), UnionValue(baseObject, other.baseObject), property)
    }
}
