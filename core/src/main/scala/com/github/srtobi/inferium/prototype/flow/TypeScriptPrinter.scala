package com.github.srtobi.inferium.prototype.flow

import com.github.srtobi.inferium.prototype.flow.Heap._

import scala.collection.{SortedSet, mutable}

class TypeScriptPrinter(val returnObject: IniEntity, val globalObject: IniEntity) {

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
    }

    private class ValueEntry(value: Value) extends Entry {
        override def print(outerGenerics: Set[String]): String = value.toString
        override def generics: Set[String] = Set()
        override def undefinedGenerics: Set[String] = Set()
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

    private val argNames = "abcdefghijklmnopqrstuvwxyz".split("")
    private class FunctionEntry(_ret: => Entry, _params: => Seq[Entry]) extends Entry {
        private lazy val ret = _ret
        private lazy val params = _params

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
            val innerGenerics = hereGenerics | outerGenerics
            val gens = hereGenerics -- outerGenerics

            val genericPrefix = buildGenericDecl(gens)
            val paramTypes = params.map(_.print(innerGenerics))
            val returnString = ret.print(innerGenerics)

            property match {
                case Some(prop) =>
                    val paramsString = paramTypes.zip(argNames).map{ case (ty, name) => s"$name: $ty" }.mkString(", ")
                    s"$prop$genericPrefix($paramsString): $returnString"
                case None =>
                    val paramsString = paramTypes.mkString(", ")
                    s"$genericPrefix($paramsString) => $returnString"
            }
        }

        private lazy val hereGenerics = calcGenerics(ret +: params, mutable.Set())
        private val gatherer = new GenericGatherer(this, ret +: params)
        override def generics: Set[String] = gatherer()
        override def generics(seen: mutable.Set[Entry]): Set[String] = gatherer(seen)
        override def undefinedGenerics: Set[String] = Set()
    }

    private class InterfaceEntry(_members: => Map[String, Entry], _function: => Option[FunctionEntry], allGenericsUndefined: Boolean) extends Entry {
        private lazy val members = _members
        private lazy val function = _function
        private lazy val name = s"I${nextId()}"
        private var printed = false

        override def print(outerGenerics: Set[String]): String = {
            printGlobal()

            val gens = hereGenerics.toSeq.sorted.map(g => if (outerGenerics(g)) g else "any")
            val genericPrefix = if (gens.nonEmpty) gens.mkString("<", ", ", ">") else ""

            return name + genericPrefix
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

        private val gatherer = new GenericGatherer(this, function.toSeq ++ members.values)
        override def generics: Set[String] = gatherer()
        override def generics(seen: mutable.Set[Entry]): Set[String] = gatherer(seen)
        private val undefinedGenericGatherer = new UndefinedGenericGatherer(this, function.toSeq ++ members.values)
        override def undefinedGenerics: Set[String] = if (allGenericsUndefined) generics else undefinedGenericGatherer()
        override def undefinedGenerics(seen: mutable.Set[Entry]): Set[String] = if (allGenericsUndefined) generics else undefinedGenericGatherer(seen)
        private def hereGenerics = undefinedGenerics
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

    private def makeEntry(entity: IniEntity): Entry = {
        entity match {
            case IniValue(value) => new ValueEntry(value)
            case IniUnion(values) => new UnionEntry(values.toSeq.map(makeEntry))
            case obj: IniObject =>
                entries.getOrElseUpdate(obj, {
                    val isInterface = obj.functionInfo.isEmpty || obj.members.nonEmpty
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
