package inferium.lattice
import inferium.lattice.assertions.Assertion
import inferium.lattice.assertions.Assertion.Effect
import inferium.typescript.{IniEntity, IniGeneric, IniNever}
import inferium.utils.macros.blockRec

import scala.collection.mutable

class ProbeEntity extends Entity with Callable {
    val _properties = mutable.Map.empty[String, IniEntity]

    private val _dynReads = mutable.Set.empty[ProbeEntity]
    private val _numberReads = mutable.Set.empty[ProbeEntity]
    private var _dynWrites: IniEntity = IniNever
    private var _numberWrites: IniEntity = IniNever
    val _calls = mutable.Map.empty[ProbeEntity, (Seq[IniEntity], IniEntity)]
    private val _constructors = mutable.Map.empty[ProbeEntity, Seq[IniEntity]]

    /*def entities: Iterator[Entity] = {
        _reads.values.iterator.flatMap(_.iterator) ++
            _writes.values.iterator ++
            _dynReads.iterator ++
            _numberReads.iterator ++
            Iterator(_dynWrites) ++
            Iterator(_numberWrites) ++
            _calls.flatMap {
                case (ret, (args, ths)) => Iterator(ret, ths) ++ args.iterator
            } ++
            _constructors.flatMap {
                case (ret, args) => Iterator(ret) ++ args.iterator
            }
    }*/


    def read(property: String, probe: ProbeEntity): Unit = {
        _properties += property -> (_properties.getOrElseUpdate(property, IniNever) | new IniGeneric(probe))
    }
    def write(property: String, value: IniEntity): Unit = {
        _properties += property -> (_properties.getOrElseUpdate(property, IniNever) | value)
    }
    def dynRead(probe: ProbeEntity): Unit = _dynReads += probe
    def numberRead(probe: ProbeEntity): Unit = _numberReads += probe
    def dynWrite(value: IniEntity): Unit = {
        _dynWrites |= value
    }
    def numberWrite(value: IniEntity): Unit = {
        _numberWrites |= value
    }
    def call(thisEntity: IniEntity, arguments: Seq[IniEntity], probe: ProbeEntity): Unit = {
        _calls += probe -> (arguments, thisEntity)
    }
    def construct(arguments: Seq[IniEntity], probe: ProbeEntity): Unit = {
        _constructors += probe -> arguments
    }


    override def isNormalized: Boolean = true
    @blockRec(nonrec = true)
    override def normalized(heap: Heap.Mutator): Entity = this
    @blockRec(nonrec = true)
    override def asBoolLattice(heap: Heap.Mutator): GeneralBoolLattice = BoolLattice.Top
    @blockRec(nonrec = true)
    override def asStringLattice(heap: Heap.Mutator): StringLattice = StringLattice.Top
    @blockRec(nonrec = true)
    override def asTypeof(heap: Heap.Mutator): Set[String] = Set("any")
    @blockRec(nonrec = true)
    override def asProbes(heap: Heap.Mutator): Seq[ProbeEntity] = Seq(this)
    @blockRec(nonrec = true)
    override protected[lattice] def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Boolean, Effect) = (this, false, Assertion.noEffect(this))

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = Seq.empty
    override def coerceToConstructionObject(heap: Heap.Mutator, constructionObject: ObjectLike): Seq[ObjectLike] = Seq.empty
    override def coerceToCallables(heap: Heap.Mutator, fail: () => Unit): Seq[Callable] = Seq(this)
}
