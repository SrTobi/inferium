package com.github.srtobi.inferium.cli

import com.github.srtobi.inferium.core.flowgraph.FunctionBuilder


object TestCli {
  def main(args: Array[String]): Unit = {
    val fb = FunctionBuilder.newProgram()
    val block = fb.mainBlock
    block.newVar("test")
    val test = block.toRegisterRef(block.findVar("test"))

    val prop = block.newReadProperty(test, block.value("prop"))

    val (succ, fail) = block.newConditional(test)
    succ.newVar("test")
    val test2 = succ.findVar("test")
    succ.newWriteProperty(test2, block.value("newProp"), prop)
    succ.finish()

    fail.newWriteProperty(test, block.value("another"), prop)
    fail.finish()
    val f = fb.finish()

    f.print(println, printExeptTarget = true)
  }
}


object BlubTest {

  import scala.reflect.runtime.{universe => ru}

  sealed abstract class Base
  case class Child1(child: Base) extends Base
  sealed class Child2 extends Base
  case class Child3(test: String) extends Child2()

  def instantiate[T](classArgs: List[AnyRef])(implicit m : Manifest[T]) : T ={
    val constructor = m.runtimeClass.getConstructors()(0)
    constructor.newInstance(classArgs:_*).asInstanceOf[T]
  }

  def getConstructorParams(clazz: ru.ClassSymbol): Seq[ru.ClassSymbol] = clazz.primaryConstructor.asMethod.paramLists.flatten.map(_.asTerm.typeSignature.typeSymbol.asClass)

  def allPossibleSubclasses(clazz: ru.ClassSymbol): Set[ru.ClassSymbol] = (clazz.knownDirectSubclasses.map(_.asClass).flatMap(allPossibleSubclasses) + clazz).filter(!_.isAbstract)

  def createRandom(ty: ru.Type, depth: Int): Unit = {
    if (depth > 4)
      return
    val clazz = ty.typeSymbol.asClass
    val subs = allPossibleSubclasses(clazz)
    println(s"creating $clazz... possible subclasses are:")
    subs.foreach(s => {
      println(s"- $s")
      val params = getConstructorParams(s)
      params.foreach(p => println(s"  -> $p"))
    })

    //subs.map(_.toType).foreach(createRandom(_, depth + 1))
  }

  def main(args: Array[String]): Unit = {
    val ty = ru.typeOf[Base]
    val ty2 = ru.typeOf[BlubTest.type]
    //val subs = ty.typeSymbol.asClass.m

    createRandom(ty, 0)
    //val test = scala.reflect.runtime.currentMirror.reflectClass(ty.typeSymbol.asClass).reflectConstructor(ty.typeSymbol.asClass.primaryConstructor.asMethod)("test").asInstanceOf[Child1]
    //println(test)
    //subs.foreach(sym => println(sym.asClass))
  }
}