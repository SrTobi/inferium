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

    private def calcGenerics(generics: Seq[Entry]): (Set[String], Set[String]) = {
        val counter = mutable.Map.empty[String, Int]

        val needToBeDefined = generics.map(_.undefinedGenerics).fold(Set())(_ | _).toSeq

        for (gen <- generics.map(_.generics).flatMap(_.toSeq) ++ needToBeDefined ++ needToBeDefined) {
            val oldCount = counter.getOrElse(gen, 0)
            counter.update(gen, oldCount + 1)
        }

        val multipleTimes = counter.filter { case (_, n) => n > 1}.keySet
        val onlyOnce = counter.filter { case (_, n) => n == 1}.keySet
        (onlyOnce, multipleTimes)
    }

    private abstract class Entry {
        def printAsProperty(property: String, outerGenerics: Set[String]): String = s"$property: ${print(outerGenerics)}"
        def print(outerGenerics: Set[String]): String
        def generics: Set[String]
        def undefinedGenerics: Set[String]
    }

    private class ValueEntry(value: Value) extends Entry {
        override def print(outerGenerics: Set[String]): String = value.toString
        override def generics: Set[String] = Set()
        override def undefinedGenerics: Set[String] = Set()
    }

    private val argNames = "abcdefghijklmnopqrstuvwxyz".split("")
    private class FunctionEntry(_ret: => Entry, _params: => Seq[Entry]) extends Entry {
        private lazy val ret = _ret
        private lazy val params = _params
        override def printAsProperty(property: String, outerGenerics: Set[String]): String = {
            return print(outerGenerics, Some(property))
        }
        override def print(outerGenerics: Set[String]): String = {
            return print(outerGenerics, None)
        }

        private def print(outerGenerics: Set[String], property: Option[String]): String = {
            val innerGenerics = hereGenerics | outerGenerics
            val gens = SortedSet((hereGenerics -- outerGenerics).toSeq: _*)

            val genericPrefix = if (gens.nonEmpty) gens.mkString("<", ", ", ">") else ""
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

        private val (singleGenerics, hereGenerics) = calcGenerics(ret +: params)
        override val generics: Set[String] = singleGenerics | hereGenerics
        override def undefinedGenerics: Set[String] = Set()
    }

    private class InterfaceEntry(_members: => Map[String, Entry], _function: => Option[FunctionEntry]) extends Entry {
        private lazy val members = _members
        private lazy val function = _function
        private lazy val name = s"I${nextId()}"
        private var printed = false

        override def print(outerGenerics: Set[String]): String = {
            printGlobal()

            val innerGenerics = hereGenerics | outerGenerics
            val gens = SortedSet((hereGenerics -- outerGenerics).toSeq: _*)

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

            val genPostfix = if (hereGenerics.nonEmpty) SortedSet(hereGenerics.toSeq: _*).mkString("<", ", ", ">") else ""
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

        private val (singleGenerics, hereGenerics) = calcGenerics(function.toSeq ++ members.values)
        override val generics: Set[String] = singleGenerics | hereGenerics
        override def undefinedGenerics: Set[String] = Set()
    }

    private class GenericEntry() extends Entry {
        private val name = s"T${nextId()}"
        override def print(outerGenerics: Set[String]): String = {
            if (outerGenerics.contains(name)) {
                return name
            } else {
                return "any"
            }
        }

        override def generics: Set[String] = Set(name)
        override def undefinedGenerics: Set[String] = Set()
    }

    private class UnionEntry(_values: => Seq[Entry]) extends Entry {
        private lazy val values = _values
        override def print(outerGenerics: Set[String]): String = values.map(_.print(outerGenerics)).mkString(" | ")
        private val (singleGenerics, hereGenerics) = calcGenerics(values)

        override def undefinedGenerics: Set[String] = hereGenerics
        override def generics: Set[String] = singleGenerics | hereGenerics
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
                            new GenericEntry()
                        } else {
                            new InterfaceEntry(obj.members.mapValues(makeEntry), obj.functionInfo.map(makeFunctionEntry))
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
        addLine(s"export = ${exportEntry.print(Set())}")
        builder.toString
    }

    def print(): String = result
}
