package inferium.lattice
import inferium.lattice.assertions.Assertion
import inferium.lattice.assertions.Assertion.Effect
import inferium.utils.macros.blockRec

import scala.collection.mutable

class ProbeEntity extends Entity with Callable {
    private val _reads = mutable.Map.empty[String, mutable.Set[ProbeEntity]]
    private val _writes = mutable.Map.empty[String, Entity]
    private val _dynReads = mutable.Set.empty[ProbeEntity]
    private val _numberReads = mutable.Set.empty[ProbeEntity]
    private var _dynWrites: Entity = NeverValue
    private var _numberWrites: Entity = NeverValue
    private val _calls = mutable.Map.empty[ProbeEntity, (Seq[Entity], Entity)]
    private val _constructors = mutable.Map.empty[ProbeEntity, Seq[Entity]]

    def entities: Iterator[Entity] = {
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
    }

    def read(property: String, probe: ProbeEntity): Unit = {
        _reads.getOrElseUpdate(property, mutable.Set.empty) += probe
    }
    def write(property: String, value: Entity): Unit = {
        assert(value.isNormalized)
        val newValue = _writes.getOrElse(property, NeverValue) unify value
        _writes += property -> newValue
    }
    def dynRead(probe: ProbeEntity): Unit = _dynReads += probe
    def numberRead(probe: ProbeEntity): Unit = _numberReads += probe
    def dynWrite(value: Entity): Unit = {
        assert(value.isNormalized)
        _dynWrites |= value
    }
    def numberWrite(value: Entity): Unit = {
        assert(value.isNormalized)
        _numberWrites |= value
    }
    def call(thisEntity: Entity, arguments: Seq[Entity], probe: ProbeEntity): Unit = {
        assert(arguments.forall(_.isNormalized))
        _calls += probe -> (arguments, thisEntity)
    }
    def construct(arguments: Seq[Entity], probe: ProbeEntity): Unit = {
        assert(arguments.forall(_.isNormalized))
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
    override def asProbes(heap: Heap.Mutator): Seq[ProbeEntity] = Seq(this)
    @blockRec(nonrec = true)
    override protected[lattice] def gatherAssertionEffects(assertion: Assertion, heap: Heap.Mutator): (Entity, Boolean, Effect) = (this, false, Assertion.noEffect(this))

    override def coerceToObjects(heap: Heap.Mutator): Seq[ObjectLike] = Seq.empty
    override def coerceToConstructionObject(heap: Heap.Mutator, constructionObject: ObjectLike): Seq[ObjectLike] = Seq.empty
    override def coerceToCallables(heap: Heap.Mutator, fail: () => Unit): Seq[Callable] = Seq(this)
}
