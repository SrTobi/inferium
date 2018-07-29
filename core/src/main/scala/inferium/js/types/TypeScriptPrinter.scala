package inferium.js.types

import inferium.js.types.js._
import inferium.lattice.ProbeEntity
import inferium.utils.IdentityKey

import scala.collection.mutable
import scala.util.Try

object TypeScriptPrinter {


    private class Printer {

        private var _nextId = 0
        private def nextId(): Int = {
            _nextId += 1
            return _nextId
        }
        private val builder = new StringBuilder()

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
            def end(): Unit = Printer.this.builder.append(builder)
        }

        private val interfaceNames = mutable.Map.empty[CompoundType, String]
        def getName(c: CompoundType): String = interfaceNames.getOrElseUpdate(c, "I" + nextId())
        private val printedInterfaces = mutable.Set.empty[CompoundType]

        class Generic(val bound: Type, var isRead: Boolean, var isWrite: Boolean) {
            val name: String = "T" + nextId()
            private var _next: Option[Generic] = None
            def get(): Generic = _next map { n => _next = Some(n.get()); _next.get } getOrElse this

            def intersect(other: Generic): Generic = {
                val me = get()
                val he = other.get()
                val newBound = me.bound.intersectWith(he.bound)
                val intersection = new Generic(newBound, me.isRead || he.isRead, me.isWrite || he.isWrite)
                me._next = Some(intersection)
                he._next = Some(intersection)
                intersection
            }
        }

        private val _generics = mutable.Map.empty[ProbeEntity, Generic]
        private def generics(probe: ProbeType): Generic = {
            _generics(probe.probe).get()
        }


        private def gatherGenerics(ty: Type, found: Set[Type]): Set[Generic] = {
            if (found(ty)) {
                return Set.empty
            }
            val innerFound = found + ty

            ty match {
                case obj: CompoundType =>
                    obj.allProperties.values.flatMap {
                        p => gatherGenerics(p.ty, innerFound)
                    }.toSet ++
                        gatherGenericsFromSignature(obj.signature ++ obj.constructor, found)

                case intersect: IntersectionType =>
                    intersect.types.flatMap(gatherGenerics(_, innerFound)).toSet
                case probe: ProbeType =>
                    val g = generics(probe)
                    gatherGenerics(g.bound, innerFound) + g
                case union: UnionType =>
                    union.types.flatMap(gatherGenerics(_, innerFound)).toSet
                case tuple: TupleType =>
                    tuple.members.flatMap(gatherGenerics(_, innerFound)).toSet
                case _ =>
                    Set.empty
            }
        }

        private def gatherGenericsFromSignature(ty: Signature, found: Set[Type]): Set[Generic] = {
            ty.flatMap {
                o =>
                    gatherGenericsFromOverload(o, found)
            }.toSet
        }

        private def gatherGenericsFromOverload(s: Overload, found: Set[Type]): Set[Generic] = {
            gatherGenerics(s.returnType, found) ++
                s.params.flatMap  {
                    p => gatherGenerics(p.ty, found)
                }
        }

        private val neededGenericsC = mutable.Map.empty[CompoundType, mutable.Set[Generic]]
        private val neededGenericsS = mutable.Map.empty[Overload, mutable.Set[Generic]]

        def gatherNeededGenerics(obj: CompoundType): Set[Generic] = {
            val foundThis: Set[Type] = Set(obj)

            var found = obj.allProperties.values.flatMap {
                p => gatherGenerics(p.ty, foundThis)
            }

            found ++= (obj.signature ++ obj.constructor).flatMap {
                o =>
                    gatherGenericsFromOverload(o, foundThis)
            }

            found.groupBy(identity).collect {case (g, l) if l.size >= 2 => g }.toSet
        }

        def gatherNeededGenerics(overload: Overload, from: CompoundType): Set[Generic] = {
            val foundThis: Set[Type] = Set(from)
            val foundGens = gatherGenerics(overload.returnType, foundThis).toSeq ++
                overload.params.flatMap  {
                    p => gatherGenerics(p.ty, foundThis)
                }

            foundGens.groupBy(identity).collect {case (g, l) if l.size >= 2 => g }.toSet
        }

        def markNeededGenerics(ty: Type, needed: Set[Generic], visited: Set[Type]): Unit = {
            if (visited.contains(ty)) {
                return
            }
            def innerVisited = visited + ty

            ty match {
                case obj: CompoundType =>
                    val innerNeeded = needed | gatherNeededGenerics(obj)
                    neededGenericsC.getOrElseUpdate(obj, mutable.Set.empty) ++= innerNeeded

                    obj.allProperties.values foreach {
                        p => markNeededGenerics(p.ty, innerNeeded, innerVisited)
                    }

                    (obj.signature ++ obj.constructor).foreach {
                        s =>
                            val overlNeeded = innerNeeded | gatherNeededGenerics(s, obj)
                            neededGenericsS.getOrElseUpdate(s, mutable.Set.empty) ++= overlNeeded
                            markNeededGenerics(s.returnType, overlNeeded, innerVisited)
                            s.params foreach {
                                p => markNeededGenerics(p.ty, overlNeeded, innerVisited)
                            }
                    }

                case intersect: IntersectionType =>
                    intersect.types foreach {
                        markNeededGenerics(_, needed, innerVisited)
                    }
                case probe: ProbeType =>
                    markNeededGenerics(generics(probe).bound, needed, innerVisited)
                case union: UnionType => union.types.foreach(markNeededGenerics(_, needed, innerVisited))
                case gen: GenericType =>
                case ObjectType =>
                case ThisType =>
                case AnyType =>
                case _: Instantiate =>
                case tuple: TupleType => tuple.members.foreach(markNeededGenerics(_, needed, innerVisited))
                case _: Primitive =>
            }
        }

        def markGenerics(ty: Type, read: Boolean, write: Boolean, visited: Set[IdentityKey[Type]]): Unit = {
            if (visited.contains(new IdentityKey[Type](ty)) && !ty.isInstanceOf[ProbeType]) {
                return
            }
            def innerVisited = visited + new IdentityKey[Type](ty)

            ty match {
                case obj: CompoundType =>
                    obj.allProperties.values foreach {
                        p => markGenerics(p.ty, read, write, innerVisited)
                    }

                    (obj.signature ++ obj.constructor).foreach {
                        s =>
                            markGenerics(s.returnType, read = true, write = write, innerVisited)
                            s.params foreach {
                                p => markGenerics(p.ty, read = read, write = true, innerVisited)
                            }
                    }

                case intersect: IntersectionType =>
                    var generic: Generic = null
                    intersect.types foreach { markGenerics(_, read, write, innerVisited) }
                    //if (write) {
                    intersect.types foreach {
                        case ty: ProbeType =>
                            val hereGen = generics(ty)
                            if (generic == null) {
                                generic = hereGen
                            } else {
                                generic = generic intersect hereGen
                            }
                        case _ =>
                    }
                //}
                case probe: ProbeType =>
                    val t = _generics.getOrElseUpdate(probe.probe, {
                        val g =new Generic(probe.bound, read, write)
                        markGenerics(g.bound, read, write, innerVisited)
                        g
                    }).get()
                    t.isRead ||= read
                    t.isWrite ||= write
                case union: UnionType => union.types.foreach(markGenerics(_, read, write, innerVisited))
                case gen: GenericType =>
                case ObjectType =>
                case ThisType =>
                case AnyType =>
                case _: Instantiate =>
                case tuple: TupleType => tuple.members.foreach(markGenerics(_, read, write, innerVisited))
                case _: Primitive =>
            }
        }

        def markDependendGenerics(ty: Type, needed: Set[Generic], visited: Set[Type]): Unit = {
            if (visited.contains(ty)) {
                return
            }
            def innerVisited = visited + ty

            ty match {
                case obj: CompoundType =>
                    val innerNeeded = needed | gatherNeededGenerics(obj)
                    neededGenericsC.getOrElseUpdate(obj, mutable.Set.empty) ++= innerNeeded

                    obj.allProperties.values foreach {
                        p => markNeededGenerics(p.ty, innerNeeded, innerVisited)
                    }

                    (obj.signature ++ obj.constructor).foreach {
                        s =>
                            val overlNeeded = innerNeeded | gatherNeededGenerics(s, obj)
                            neededGenericsS.getOrElseUpdate(s, mutable.Set.empty) ++= overlNeeded
                            markNeededGenerics(s.returnType, overlNeeded, innerVisited)
                            s.params foreach {
                                p => markNeededGenerics(p.ty, overlNeeded, innerVisited)
                            }
                    }

                case intersect: IntersectionType =>
                    intersect.types foreach {
                        markNeededGenerics(_, needed, innerVisited)
                    }
                case probe: ProbeType =>
                    markNeededGenerics(generics(probe).bound, needed, innerVisited)
                case union: UnionType => union.types.foreach(markNeededGenerics(_, needed, innerVisited))
                case gen: GenericType =>
                case ObjectType =>
                case ThisType =>
                case AnyType =>
                case _: Instantiate =>
                case tuple: TupleType => tuple.members.foreach(markNeededGenerics(_, needed, innerVisited))
                case _: Primitive =>
            }
        }



        def hereGenerics(c: CompoundType): Set[Generic] = neededGenericsC(c).toSet & gatherGenerics(c, Set.empty)
        def hereGenerics(o: Overload, c: CompoundType): Set[Generic] = neededGenericsS(o).toSet & gatherGenericsFromOverload(o, Set(c))
        def printParam(arg: Param, outerGenerics: Set[Generic]): String = arg.name + (if (arg.optional) "?" else "") + ": " + printInline(arg.ty, outerGenerics)

        def genericHeader(generics: Set[Generic], outerGenerics: Set[Generic]): String = {
            val genericsInList = generics -- outerGenerics
            if (genericsInList.isEmpty) {
                return ""
            }
            genericsInList.toSeq.sortBy(_.name).map {
                g =>
                    assert(g.get() == g)
                    val extend = printInline(g.bound, outerGenerics | generics)
                    g.name + (if (extend == "any" || extend == "{}") "" else " extends " + extend)
            }.mkString("<", ", ", ">")
        }

        def genericInst(generics: Set[Generic], outerGenerics: Set[Generic]): String = {
            if (generics.isEmpty) {
                return ""
            }

            generics.toSeq.sortBy(_.name).map {
                g =>
                    assert(g.get() == g)
                    if (outerGenerics(g)) {
                        g.name
                    } else {
                        printInline(g.bound, outerGenerics | generics)
                    }
            }.mkString("<", ", ", ">")
        }

        def printGlobal(compoundType: CompoundType): Unit = {
            if (printedInterfaces(compoundType)) {
                return
            }
            printedInterfaces += compoundType

            val hereGenerics = this.hereGenerics(compoundType)

            val writer = new Writer
            writer.print("interface " + getName(compoundType) + genericHeader(hereGenerics, Set.empty) + " {")

            val innerWriter = writer.indent(2)

            compoundType.signature foreach {
                o =>
                    printOverload(innerWriter, o, compoundType, hereGenerics, "")
            }

            compoundType.constructor foreach {
                o =>
                    printOverload(innerWriter, o, compoundType, hereGenerics, "new ")
            }

            compoundType.allProperties foreach {
                case (_, prop) =>
                    printProperty(innerWriter, prop, compoundType, hereGenerics)
            }

            writer.print("}")
            writer.end()
        }

        def printOverload(writer: Writer, o: Overload, compoundType: CompoundType, outerGenerics: Set[Generic], prefix: String): Unit = {
            val allFuncGens = this.hereGenerics(o, compoundType)
            val genList = genericHeader(allFuncGens, outerGenerics)
            writer.print(prefix + genList + o.params.map(printParam(_, allFuncGens)).mkString("(", ", ", ")") + ": " + printInline(o.returnType, allFuncGens))
        }

        def makePropertyName(name: String): String = if (Try(name.toLong).isSuccess) s"[$name]" else name

        def printProperty(writer: Writer, prop: Property, compoundType: CompoundType, generics: Set[Generic]): Unit = {
            prop.ty match {
                case func: CompoundType if func.constructor.isEmpty && func.ownProperties.isEmpty && !prop.optional =>
                    func.signature foreach {
                        printOverload(writer, _, compoundType, generics, makePropertyName(prop.name))
                    }

                case ty =>
                    val readonly = if (prop.readonly) "readonly " else ""
                    val optional = if (prop.optional) "?" else ""
                    writer.print(readonly + makePropertyName(prop.name) + optional + ": " + printInline(prop.ty, generics))
            }
        }

        def printInline(ty: Type, outerGenerics: Set[Generic]) : String = {
            ty match {
                case NeverType => "never"
                case UndefinedType => "undefined"
                case NullType => "null"
                case NumberType => "number"
                case StringType => "string"
                case ObjectType => "object"
                case ThisType => "this"
                case BooleanType => "boolean"
                case TrueType => "true"
                case FalseType => "false"
                case AnyType => "any"
                case LiteralType(value) => '"' + value + '"'
                case tuple: TupleType => tuple.members.map(printInline(_, outerGenerics)).mkString("[", ", ", "]")
                case union: UnionType => union.types.map(printInline(_, outerGenerics)).mkString("(", " | ", ")")
                case intersection: IntersectionType => intersection.types.map(printInline(_, outerGenerics)).mkString("(", " & ", ")")
                case inst: Instantiate =>
                    if (inst.typeArguments.isEmpty) {
                        printInline(inst.target, outerGenerics)
                    } else {
                        inst.target.name.get + inst.typeArguments.map(printInline(_, outerGenerics)).mkString("<", ", ", ">")
                    }
                case obj: CompoundType =>
                    if (obj.constructor.isEmpty && obj.signature.isEmpty && obj.allProperties.isEmpty) {
                        "{}"
                    } else {
                        printGlobal(obj)
                        getName(obj) + genericInst(hereGenerics(obj), outerGenerics)
                    }

                case _: GenericType => "generic"
                case p: ProbeType =>
                    val g = generics(p)
                    if (/*(g.isWrite && g.isRead) || */outerGenerics(g)) {
                        g.name
                    } else {
                        printInline(g.bound, outerGenerics)
                    }
            }
        }

        def print(ty: Type): String = {
            markGenerics(ty, read = true, write = true, Set.empty)
            markNeededGenerics(ty, Set.empty, Set.empty)
            val res = "\n=> " + printInline(ty, Set.empty)
            builder.toString() + res
        }
    }

    def print(ty: Type): String = new Printer().print(ty)
}
