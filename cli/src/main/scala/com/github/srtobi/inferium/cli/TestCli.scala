package com.github.srtobi.inferium.cli

import com.github.srtobi.inferium.SimpleTest

object TestCli {
  def main(args: Array[String]): Unit = {
    val x = new SimpleTest()
    println(x.multiply(3))
  }
}
