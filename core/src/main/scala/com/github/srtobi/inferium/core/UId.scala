package com.github.srtobi.inferium.core

import scala.collection.mutable

class UId private (val num: Int, val group: String) {
  def id: String = s"$group[$num]"

  override def toString: String = id
}

object UId {
  private val nextIdByGroups = mutable.HashMap.empty[String, Int]
  def apply(group: String): UId = {
    val cur = nextIdByGroups.getOrElseUpdate(group, 1)
    nextIdByGroups(group) = cur + 1
    return new UId(cur, group)
  }
}
