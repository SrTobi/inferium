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

    f.print(println, true)
  }
}
