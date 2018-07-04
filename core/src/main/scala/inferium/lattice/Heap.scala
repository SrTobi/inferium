package inferium.lattice

import inferium.Config.ConfigKey
import inferium.Unifiable
import inferium.Unifiable.Fixpoint
import inferium.lattice.Heap.{Mutator, Shared}
import inferium.lattice.Heap.SpecialObjects.SpecialObject

import scala.collection.mutable
import scala.language.implicitConversions


abstract class Heap(protected val shared: Shared) extends Unifiable[Heap] {
    final def config: Heap.Config = shared.config
    final def specialObject(specialObject: SpecialObject): ObjectLike = shared.specialObjects(specialObject)

    def begin(location: Location): Mutator
    def end(actor: Mutator): Heap

    def split(): Heap

    override def unify(other: Heap)(implicit fixpoint: Fixpoint): Heap = unify(Seq(other))
    override def unify(heaps: Seq[Heap])(implicit fixpoint: Fixpoint): Heap
}


object Heap {
    object SpecialObjects extends Enumeration {
        type SpecialObject = Value
        val Object, ObjectConstructor, Function, FunctionConstructor = Value
    }

    type SpecialObjectMap = mutable.Map[SpecialObject, ObjectLike]
    case class Shared(config: Config, specialObjects: SpecialObjectMap = mutable.Map.empty)

    sealed class PropertyMutationResult
    case class SuccessfulPropertyMutation(result: Ref) extends PropertyMutationResult

    case class Config(dynamicWriteAffectsExistingProperties: Boolean,
                      dynamicWriteAffectsOnlyConfigurable: Boolean,
                      dynamicWriteAffectsOnlyEnumerable: Boolean,
                      dynamicReadRespectsProperties: Boolean,
                      dynamicReadRespectsPrototypes: Boolean)
    object Config extends inferium.Config.Section("Heap") {

        val dynamicWriteAffectsExistingProperties: ConfigKey[Boolean] = ConfigKey(true)
        val dynamicWriteAffectsOnlyConfigurable: ConfigKey[Boolean] = ConfigKey(true)
        val dynamicWriteAffectsOnlyEnumerable: ConfigKey[Boolean] = ConfigKey(true)
        val dynamicReadRespectsProperties: ConfigKey[Boolean] = ConfigKey(true)
        val dynamicReadRespectsPrototypes: ConfigKey[Boolean] = ConfigKey(false)
        


        implicit def configToHeapConfig(config: inferium.Config): Heap.Config = Heap.Config(
            dynamicWriteAffectsExistingProperties = config(dynamicWriteAffectsExistingProperties),
            dynamicWriteAffectsOnlyConfigurable = config(dynamicWriteAffectsOnlyConfigurable),
            dynamicWriteAffectsOnlyEnumerable = config(dynamicWriteAffectsOnlyEnumerable),
            dynamicReadRespectsProperties = config(dynamicReadRespectsProperties),
            dynamicReadRespectsPrototypes = config(dynamicReadRespectsPrototypes)
        )
    }

    abstract class Mutator {
        final def config: Config = origin.config
        def origin: Heap

        def allocObject(location: Location, creator: (Location, Long) => ObjectLike, prototype: Entity): ObjectLike
        def allocOrdinaryObject(location: Location, prototype: Entity): ObjectLike = allocObject(location, (loc, ac) => OrdinaryObjectEntity(loc)(ac), prototype)
        def allocOrdinaryObject(location: Location): ObjectLike = {
            val objectPrototype = origin.specialObject(SpecialObjects.Object)
            allocOrdinaryObject(location, objectPrototype)
        }
        def isConcreteObject(obj: ObjectLike): Boolean
        def setProperty(obj: ObjectLike, propertyName: String, property: ConcreteProperty): Unit
        def writeToProperties(obj: ObjectLike, valueLocs: ValueLocation, numbersOnly: Boolean, resolvedValue: Entity): Unit
        def writeToProperty(obj: ObjectLike, propertyName: String, valueLocs: ValueLocation, isCertainWrite: Boolean, resolvedValue: Entity): Property
        def getProperties(obj: ObjectLike, numbersOnly: Boolean): TraversableOnce[(Option[String], Property)]
        def getProperty(obj: ObjectLike, propertyName: String): Property
        //def listProperties(obj: ObjectLike): Seq[Entity]

        def getValue(loc: ValueLocation): Entity
        def setValue(loc: Location, value: Entity): ValueLocation
        def changeValue(loc: ValueLocation, value: Entity): Unit

        def getPropertyValueIgnoringGetter(obj: ObjectLike, propertyName: String): Entity = {
            getProperty(obj, propertyName) match {
                case ConcreteProperty(_, _, value, _, getter, _) =>
                    val result = value.toSeq map getValue
                    UnionValue(result)

                case AbstractProperty(_, _, value, _, _, _, mightBeAbsent) =>
                    if (mightBeAbsent)
                        value | UndefinedValue
                    else
                        value
            }
        }

        def forceSetPropertyValue(obj: ObjectLike, propertyName: String, writeLoc: Location, value: Entity): Unit = {
            val valueLocation = setValue(writeLoc, value)
            setProperty(obj, propertyName, ConcreteProperty.defaultWriteToObject(Set(valueLocation)))
        }
    }

    abstract class Factory {
        def create(config: Config): (Heap, SpecialObjectMap)
    }
}