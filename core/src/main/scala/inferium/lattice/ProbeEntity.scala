package inferium.lattice
import inferium.js.types.js
import inferium.js.types.js.NeverType
import inferium.lattice.assertions.Assertion
import inferium.lattice.assertions.Assertion.Effect
import inferium.typescript.IniEntity
import inferium.utils.macros.blockRec

import scala.collection.mutable

class ProbeEntity extends Entity with Callable {
    val _writes = mutable.Map.empty[String, js.Type]
    val _reads = mutable.Map.empty[String, js.Type]

    var _dynReads: js.Type = js.NeverType
    var _numberReads: js.Type = js.NeverType
    var _dynWrites: js.Type = js.NeverType
    var _numberWrites: js.Type = js.NeverType
    val _calls = mutable.Map.empty[ProbeEntity, (Seq[js.Type], js.Type)]
    val _constructors = mutable.Map.empty[ProbeEntity, Seq[js.Type]]
    var _usedAs: js.Type = NeverType

    def entities: Iterator[Entity] = {
        _reads.values.iterator.flatMap(_.origin.iterator) ++
            _writes.values.iterator.flatMap(_.origin.iterator) ++
            _dynReads.origin.iterator ++
            _numberReads.origin.iterator ++
            _dynWrites.origin.iterator ++
            _numberWrites.origin.iterator ++
            _calls.flatMap {
                case (ret, (args, ths)) => Iterator(ret) ++ ths.origin.iterator ++ args.flatMap(_.origin.iterator)
            } ++
            _constructors.flatMap {
                case (ret, args) => Iterator(ret) ++ args.iterator.flatMap(_.origin.iterator)
            } ++
            _usedAs.origin.iterator
    }

    def usedAs(ty: js.Type): Unit = {
        _usedAs = _usedAs intersectWith ty
    }

    def read(property: String, probe: ProbeEntity): Unit = {
        _reads += property -> (_reads.getOrElseUpdate(property, js.NeverType) intersectWith new js.ProbeType(probe))
    }
    def write(property: String, ty: js.Type): Unit = {
        _writes += property -> (_writes.getOrElseUpdate(property, js.NeverType) unionWith ty)
    }
    def dynRead(probe: ProbeEntity): Unit = _dynReads = _dynReads intersectWith new js.ProbeType(probe)
    def numberRead(probe: ProbeEntity): Unit = _numberReads = _numberReads unionWith new js.ProbeType(probe)
    def dynWrite(ty: js.Type): Unit = {
        _dynWrites = _dynWrites unionWith ty
    }
    def numberWrite(ty: js.Type): Unit = {
        _numberWrites = _dynWrites unionWith ty
    }
    def call(thisEntity: js.Type, arguments: Seq[js.Type], probe: ProbeEntity): Unit = {
        _calls += probe -> (arguments, thisEntity)
    }
    def construct(arguments: Seq[js.Type], probe: ProbeEntity): Unit = {
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
