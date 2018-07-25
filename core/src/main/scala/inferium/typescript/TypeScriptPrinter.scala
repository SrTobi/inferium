package inferium.typescript

import java.util

import inferium.Unifiable
import inferium.lattice._
import inferium.utils.Utils
import inferium.utils.macros.blockRec

import scala.collection.{SortedSet, mutable}
import scala.util.Try


case class IniFunctionInfo(returnValue: IniEntity, parameter: Seq[IniEntity]) {

    override lazy val hashCode: Int = recHashCode()

    override def equals(obj: scala.Any): Boolean = recEqual(obj, new util.IdentityHashMap)

    @blockRec(nonrec = true)
    def recHashCode(): Int = {
        7 * returnValue.recHashCode() + parameter.map(_.recHashCode()).foldLeft(0){11 * _ + _}
    }

    def recEqual(other: Any, objs: util.IdentityHashMap[IniObject, IniObject]): Boolean = other match {
        case other: IniFunctionInfo =>
            returnValue.recEqual(other.returnValue, objs) &&
                parameter.size == other.parameter.size &&
                parameter.zip(other.parameter).forall { case (a, b) => a.recEqual(b, objs) }
        case _ => false
    }

    def join(other: IniFunctionInfo)(implicit objs: Map[IniObject, IniRec]): IniFunctionInfo = IniFunctionInfo(
        returnValue join other.returnValue,
        parameter.zipAll(other.parameter, IniNever, IniNever) map { case (a, b) => a join b}
    )
}


sealed abstract class IniEntity {

    @blockRec
    def recHashCode(): Int

    def recEqual(other: Any, objs: util.IdentityHashMap[IniObject, IniObject]): Boolean

    override def equals(obj: scala.Any): Boolean = recEqual(obj, new util.IdentityHashMap)

    override lazy val hashCode: Int = recHashCode()

    def join(other: IniEntity)(implicit objs: Map[IniObject, IniRec]): IniEntity = IniUnion(Seq(this, other))
    def | (other: IniEntity): IniEntity = join(other)(Map.empty)
}

object IniNever extends IniEntity {
    @blockRec(nonrec = true)
    override def recHashCode(): Int = 23989489

    override def recEqual(other: Any, objs: util.IdentityHashMap[IniObject, IniObject]): Boolean = other match {
        case other: IniNever.type => true
        case _ => false
    }
}
sealed trait IniAtom extends IniEntity

sealed class IniRec(_target: => IniObject) extends IniEntity with IniAtom {
    lazy val target: IniObject = _target

    override def equals(other: Any): Boolean = other match {
        case other: IniRec =>
            other eq this
        case _ =>
            false
    }

    @blockRec(nonrec = true)
    override def recHashCode(): Int = {
        target.recHashCode()
    }

    override def recEqual(other: Any, objs: util.IdentityHashMap[IniObject, IniObject]): Boolean = other match {
        case rec: IniRec =>
            target.recEqual(rec.target, objs)
        case obj: IniObject =>
            target.recEqual(obj, objs)
        case _ =>
            false
    }

    override def toString: String = s"rec<$target>"
}

sealed class IniGeneric(val probe: ProbeEntity) extends IniEntity with IniAtom {

    @blockRec(nonrec = true)
    override def recHashCode(): Int  = probe.hashCode()

    override def recEqual(other: Any, objs: util.IdentityHashMap[IniObject, IniObject]): Boolean = other match {
        case other: IniGeneric => this eq other
        case _ => false
    }
}

class IniObject(val members: Map[String, (Boolean, IniEntity)], val functionInfo: Option[IniFunctionInfo]) extends IniEntity {

    def joinObj(other: IniObject)(implicit objs: Map[IniObject, IniRec]): IniObject = {
        new IniObject(
            Utils.mergeMapsWithMapper(members, other.members) { case (_, e) => (true, e) } { case ((opt1, e1), (opt2, e2)) => (opt1 || opt2, e1 join e2) },
            other.functionInfo map { i => functionInfo map {
                _ join i
            } getOrElse i
            } orElse functionInfo
        )
    }

    override lazy val hashCode: Int = members.values.hashCode() * (if (functionInfo.isDefined) 239 else 3923)

    @blockRec(2389)
    override def recHashCode(): Int = {
        val membersHash = members.map{ case (name, (opt, e)) => name.hashCode + opt.hashCode + e.recHashCode() }.foldLeft(0){17 * _ + _}
        val funcHash = functionInfo map { _.recHashCode() } getOrElse 3289
        membersHash + funcHash
    }

    override def recEqual(other: Any, objs: util.IdentityHashMap[IniObject, IniObject]): Boolean = other match {
        case other: IniObject =>
            if (objs.containsKey(this)) {
                return other eq objs.get(this)
            }
            objs.put(this, other)
            val result = members.size == other.members.size &&
                members.zip(other.members).forall{case ((n1, (opt1, e1)), (n2, (opt2, e2))) => n1 == n2 && opt1 == opt2 && e1.recEqual(e2, objs) } &&
                functionInfo.isDefined == other.functionInfo.isDefined &&
                (functionInfo.isEmpty || functionInfo.get.recEqual(other.functionInfo.get, objs))

            objs.remove(this)
            result
        case rec: IniRec =>
            this.recEqual(rec.target, objs)
        case _ => false
    }
}

case class IniValue(value: Primitive) extends IniEntity with IniAtom {
    @blockRec(nonrec = true)
    override def recHashCode(): Int = value.hashCode()

    override def recEqual(other: Any, objs: util.IdentityHashMap[IniObject, IniObject]): Boolean = other match {
        case other: IniValue => value == other.value
        case _ => false
    }
}
class IniUnion(val atoms: Set[IniAtom], val obj: Option[IniObject]) extends IniEntity {
    assert(atoms.size + obj.size > 1)

    def values: Seq[IniEntity] = atoms.toSeq ++ obj.toSeq

    @blockRec(nonrec = true)
    override def recHashCode(): Int = atoms.hashCode() + obj.map{ _.recHashCode() }.getOrElse(2378)

    override def recEqual(other: Any, objs: util.IdentityHashMap[IniObject, IniObject]): Boolean = other match {
        case other: IniUnion =>
            atoms.size == other.atoms.size &&
                atoms.zip(other.atoms).forall{ case (me, he) => me.recEqual(he, objs) } &&
                ((other.obj, obj) match {
                    case (Some(a), Some(b)) => a.recEqual(other, objs)
                    case _ => false
                })
        case _ => false
    }

    override def join(other: IniEntity)(implicit objs: Map[IniObject, IniRec]): IniEntity = IniUnion(values :+ other)

    override def toString: String = (atoms ++ obj.toSeq).mkString("(", "|", ")")
}

object IniUnion {
    def apply(values: Traversable[IniEntity])(implicit objs: Map[IniObject, IniRec] = Map.empty): IniEntity = {
        var obj: Option[IniObject] = None
        val rec = new IniRec(obj.get)

        def flat(e: Traversable[IniEntity]): Traversable[IniAtom] = e.flatMap {
            case IniNever => Seq.empty
            case u: IniUnion => flat(u.values)
            case o: IniObject =>
                objs get o match {
                    case Some(r) => Seq(r)
                    case None =>
                        obj match {
                            case Some(other) => obj = Some(other.joinObj(o)(objs + (o -> rec)))
                            case None => obj = Some(o)
                        }
                        Seq.empty
                }
            case r: IniRec =>
                flat(Seq(r.target))
            case p: IniAtom =>
                Seq(p)
            case _ =>
                Seq.empty
        }

        val atoms = flat(values).toSet

        if (obj.isDefined) {
            rec.target
        }

        (atoms.isEmpty, obj) match {
            case (true, Some(o)) => o
            case (false, None) if atoms.size == 1 => atoms.head
            case (_, _) => new IniUnion(atoms, obj)
        }
    }
}

object IniEntity {
    def from(entity: Entity, heap: Heap.Mutator, objs: Map[ObjectLike, IniRec] = Map.empty): IniEntity = entity.normalized(heap) match {
        case NeverValue => IniNever
        case p: Primitive => IniValue(p)
        case union: UnionValue => IniUnion(union.entities.map{from(_, heap, objs)})
        case obj: ObjectLike =>
            if (objs contains obj) {
                return objs(obj)
            }

            var iniObj: IniObject = null
            val rec = new IniRec(iniObj)

            var props = Map.empty[String, (Boolean, IniEntity)]

            heap.getProperties(obj, numbersOnly = false) foreach {
                case (Some(name), prop) =>
                    val ap = prop.abstractify(heap)
                    val newProp = from(ap.value, heap, objs + (obj -> rec))
                    props += name -> (props.get(name) match {
                        case Some(other) =>
                            (other._1 || ap.mightBeAbsent, other._2 | newProp)
                        case None =>
                            (ap.mightBeAbsent, newProp)
                    })

                case _ =>
                    // todo give out general info
            }

            val funcInfo = obj match {
                case f: FunctionEntity =>
                    val args = f.callableInfo.argumentProbe._properties.flatMap {
                        case (name, prop) if Try(name.toInt).isSuccess =>
                            Seq(name.toInt -> prop)
                        case _ =>
                            Seq.empty
                    }.toSeq.sortBy(_._1).map(_._2)

                    Some(IniFunctionInfo(IniEntity.from(f.callableInfo.returnValue, heap), args))
                case _ =>
                    None
            }

            iniObj = new IniObject(props, funcInfo)
            iniObj

        case probe: ProbeEntity =>
            new IniGeneric(probe)
    }
}



class TypeScriptPrinter(val returnObject: IniEntity) {

    import collection.Set
    import collection.Map

    private var _nextId = 0
    private def nextId(): Int = {
        _nextId += 1
        return _nextId
    }
    private val builder = new StringBuilder()
    private val entries = mutable.Map.empty[IniEntity, Entry]
    private val genericExtends = mutable.Map.empty[String, Entry]

    private def addLine(line: String): Unit = {
        builder.append(line)
        builder.append("\n")
    }

    private class Writer(val builder: mutable.StringBuilder = new mutable.StringBuilder(), val indent: Int = 0) {
        def print(line: String): Unit = {
            for (i <- 0 until indent)
                builder.append(" ")
            builder.append(line)
            print()
        }
        def print(): Unit = builder.append("\n")

        def indent(i: Int): Writer = new Writer(builder, i)
        def end(): Unit = TypeScriptPrinter.this.builder.append(builder)
    }

    private def join(sets: Seq[Set[String]]): Set[String] = sets.foldLeft(Set[String]()){_ | _}

    private class GenericGatherer(thisEntry: Entry, generics: => Seq[Entry]) {
        private var cache: Option[Set[String]] = None

        def apply(seen: mutable.Set[Entry] = mutable.Set()): Set[String] = cache match{
            case Some(res) => res
            case None =>
                if (seen(thisEntry)) {
                    return Set()
                }
                val doCaching = seen.isEmpty
                val gens = generics
                seen += thisEntry
                val result = join(gens.map(_.generics(seen)))
                if (doCaching)
                    cache = Some(result)
                result
        }
    }

    private class UndefinedGenericGatherer(thisEntry: Entry, generics: => Seq[Entry]) {
        private var cache: Option[Set[String]] = None

        def apply(seen: mutable.Set[Entry] = mutable.Set()): Set[String] = cache match{
            case Some(res) => res
            case None =>
                if (seen(thisEntry)) {
                    return Set()
                }
                val doCaching = seen.isEmpty
                val gens = generics
                seen += thisEntry
                val result = calcGenerics(gens, seen)
                if (doCaching)
                    cache = Some(result)
                result
        }
    }

    private def calcGenerics(generics: Seq[Entry], seen: mutable.Set[Entry]): Set[String] = {
        val counter = mutable.Map.empty[String, Int]

        val needToBeDefined = join(generics.map(_.undefinedGenerics(seen.clone())))

        for (gen <- generics.map(_.generics).flatMap(_.toSeq) ++ needToBeDefined ++ needToBeDefined) {
            val oldCount = counter.getOrElse(gen, 0)
            counter.update(gen, oldCount + 1)
        }

        return counter.filter { case (_, n) => n > 1}.keySet
    }

    private abstract class Entry {
        def printAsExport(): String = s"var exp: ${print(Set())}\nexport = exp"
        def printAsProperty(property: String, outerGenerics: Set[String]): String = s"$property: ${print(outerGenerics)}"
        def print(outerGenerics: Set[String]): String
        def generics: Set[String]
        def generics(seen: mutable.Set[Entry]): Set[String] = generics
        def undefinedGenerics: Set[String]
        def undefinedGenerics(seen: mutable.Set[Entry]): Set[String] = undefinedGenerics
        def member: Seq[Entry]
    }

    private class ValueEntry(value: String) extends Entry {
        override def print(outerGenerics: Set[String]): String = value.toString
        override def generics: Set[String] = Set()
        override def undefinedGenerics: Set[String] = Set()

        override def member: Seq[Entry] = Seq()
    }

    private def buildGenericDecl(generics: Set[String]): String = {
        if (generics.isEmpty) {
            return ""
        } else {
            //if (hereGenerics.nonEmpty) SortedSet(hereGenerics.toSeq: _*).mkString("<", ", ", ">") else ""
            return generics.toSeq.sorted.map {
                gen =>
                    genericExtends.get(gen).map(_.print(generics.toSet)).map(superType => s"$gen extends $superType").getOrElse(gen)
            }.mkString("<", ", ", ">")
        }
    }

    private def buildGenericDef(hereGenerics: Set[String], outerGenerics: Set[String]): String = {
        val gens = hereGenerics.toSeq.sorted.map(g => if (outerGenerics(g)) g else "any")
        if (gens.nonEmpty) gens.mkString("<", ", ", ">") else ""
    }

    private val argNames = "abcdefghijklmnopqrstuvwxyz".split("")
    private class FunctionEntry(_ret: => Entry, _params: => Seq[Entry]) extends Entry {
        private lazy val ret = _ret
        private lazy val params = _params
        private var printing = false
        private var printed = false
        private lazy val id = nextId()

        override def printAsExport(): String = {
            s"declare function ${printAsProperty("func", Set())}\nexport = func"
        }

        override def printAsProperty(property: String, outerGenerics: Set[String]): String = {
            return print(outerGenerics, Some(property))
        }
        override def print(outerGenerics: Set[String]): String = {
            return print(outerGenerics, None)
        }

        private def print(outerGenerics: Set[String], property: Option[String]): String = {
            val isRecursive = printing
            printing = true
            val innerGenerics = hereGenerics | outerGenerics
            val gens = hereGenerics -- outerGenerics

            lazy val paramTypes = params.map(_.print(innerGenerics))
            lazy val returnString = ret.print(innerGenerics)
            lazy val paramsString = paramTypes.zip(argNames).map{ case (ty, name) => s"$name: $ty" }.mkString(", ")


            if (isRecursive) {
                val name = s"F$id"
                if (!printed) {
                    printed = true
                    val genericPrefix = buildGenericDecl(innerGenerics)
                    val globalString = s"type $name$genericPrefix = ($paramsString) => $returnString"
                    addLine(globalString)
                }

                assert(innerGenerics == outerGenerics)
                return s"$name" + buildGenericDef(innerGenerics, outerGenerics)
            } else {
                val genericPrefix = buildGenericDecl(gens)

                val result = property match {
                    case Some(prop) =>
                        s"$prop$genericPrefix($paramsString): $returnString"
                    case None =>
                        val paramsString = paramTypes.mkString(", ")
                        s"$genericPrefix($paramsString) => $returnString"
                }

                printing = false
                return result
            }
        }

        private lazy val hereGenerics = calcGenerics(ret +: params, mutable.Set())
        private val gatherer = new GenericGatherer(this, ret +: params)
        override def generics: Set[String] = gatherer()
        override def generics(seen: mutable.Set[Entry]): Set[String] = gatherer(seen)
        override def undefinedGenerics: Set[String] = Set()

        override def member: Seq[Entry] = ret +: params
    }

    private class InterfaceEntry(_members: => Map[String, Entry], _function: => Option[FunctionEntry], allGenericsUndefined: Boolean) extends Entry {
        private lazy val members = _members
        private lazy val function = _function
        private lazy val name = s"I${nextId()}"
        private var printed = false

        override def print(outerGenerics: Set[String]): String = {
            printGlobal()

            return name + buildGenericDef(hereGenerics, outerGenerics)
        }

        private def printGlobal(): Unit = {
            if (printed) {
                return
            }
            printed = true

            val writer = new Writer()
            val inner = writer.indent(2)

            val genPostfix = buildGenericDecl(hereGenerics)
            writer.print(s"interface $name$genPostfix {")

            function.foreach {
                info =>
                    inner.print(info.print(hereGenerics))
                    inner.print()
            }

            members.foreach {
                case (prop, ty) =>
                    inner.print(ty.printAsProperty(prop, hereGenerics))
            }
            writer.print("}")
            writer.print()
            writer.end()
        }

        private val gatherer = new GenericGatherer(this, member)
        override def generics: Set[String] = gatherer()
        override def generics(seen: mutable.Set[Entry]): Set[String] = gatherer(seen)
        private val undefinedGenericGatherer = new UndefinedGenericGatherer(this, member)
        override def undefinedGenerics: Set[String] = if (allGenericsUndefined) generics else undefinedGenericGatherer()
        override def undefinedGenerics(seen: mutable.Set[Entry]): Set[String] = if (allGenericsUndefined) generics else undefinedGenericGatherer(seen)
        private def hereGenerics = undefinedGenerics

        override def member: Seq[Entry] = function.toSeq ++ members.values
    }

    private class GenericEntry() extends Entry {
        val name = s"T${nextId()}"
        override def print(outerGenerics: Set[String]): String = {
            if (outerGenerics.contains(name)) {
                return name
            } else {
                return "any"
            }
        }

        override def generics: Set[String] = Set(name) | genericExtends.get(name).map(_.generics).getOrElse(Set())
        override def undefinedGenerics: Set[String] = genericExtends.get(name).map(_.generics + name).getOrElse(Set())

        override def member: Seq[Entry] = Seq()
    }

    private class UnionEntry(_values: => Seq[Entry]) extends Entry {
        private lazy val values = _values
        override def print(outerGenerics: Set[String]): String = values.map(_.print(outerGenerics)).mkString(" | ")

        private val gatherer = new GenericGatherer(this, values)
        override def generics: Set[String] = gatherer()
        override def generics(seen: mutable.Set[Entry]): Set[String] = gatherer(seen)


        private val undefinedGenericGatherer = new UndefinedGenericGatherer(this, values)
        override def undefinedGenerics: Set[String] = undefinedGenericGatherer()
        override def undefinedGenerics(seen: mutable.Set[Entry]): Set[String] = undefinedGenericGatherer(seen)

        override def member: Seq[Entry] = values
    }


    /*
    private def makeFuncTypeString(info: IniFunctionInfo): String = info match {
        case IniFunctionInfo(ret, params) =>
            return params.map(getIdentifierFor).zip(argNames).map {
                case (ty, name) => s"$name: $ty"
            }.mkString("(", ", ", ") => " + getIdentifierFor(ret))
    }*/

    private def makeFunctionEntry(info: IniFunctionInfo): FunctionEntry = {
        new FunctionEntry(makeEntry(info.returnValue), info.parameter.map(makeEntry))
    }

    private val generics = mutable.Map.empty[ProbeEntity, GenericEntry]

    private def makeEntry(entity: IniEntity): Entry = {
        entity match {
            case IniNever => new ValueEntry("never")
            case IniValue(value) => new ValueEntry(value.toString)
            case union: IniUnion => new UnionEntry(union.values.toSeq.map(makeEntry))
            case rec: IniRec =>
                entries(rec.target)
            case g: IniGeneric =>
                generics.getOrElseUpdate(g.probe, {
                    val generic = new GenericEntry()
                    val props = g.probe._properties.map {
                        case (name, prop) => (name, makeEntry(prop))
                    }

                    if (props.nonEmpty)
                        genericExtends += generic.name -> new InterfaceEntry(props, None, true)

                    generic
                } )
            case obj: IniObject =>
                entries.getOrElseUpdate(obj, {
                    /*val isInterface = obj.functionInfo.isEmpty || obj.members.nonEmpty
                    if (isInterface) {
                        if (obj.isUserObject || obj.members.isEmpty) {
                            val generic = new GenericEntry()
                            val info = obj.userInfo.get
                            if (info.readProperties.nonEmpty) {
                                genericExtends += generic.name -> new InterfaceEntry(info.readProperties.mapValues(makeEntry), None, true)
                            }
                            generic
                        } else {
                            new InterfaceEntry(obj.members.mapValues(makeEntry), obj.functionInfo.map(makeFunctionEntry), false)
                        }
                    } else {
                        val info = obj.functionInfo.get
                        makeFunctionEntry(info)
                    }*/

                    val props = obj.members.filter { case (name, _) => name != "prototype" || obj.functionInfo.isEmpty }.mapValues(mem => makeEntry(mem._2))
                    val func = obj.functionInfo.map(makeFunctionEntry)
                    if (func.isDefined && props.isEmpty) {
                        func.get
                    } else {
                        new InterfaceEntry(props, func, false)
                    }
                })
        }
    }

    private val result = {
        val exportEntry = makeEntry(returnObject)
        addLine(exportEntry.printAsExport())
        builder.toString
    }

    def print(): String = result
}

