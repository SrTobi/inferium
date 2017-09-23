package com.github.srtobi.inferium


object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello world! " + new SimpleTest().multiply(3))
  }
}