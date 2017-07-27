package com.wix.exams.tree

import com.wix.exams.tree.BinTree._

import scala.util.control.NonFatal

class BinTree(val value: String, var left: Option[BinTree], var right: Option[BinTree]) {

  @throws[SerializationException]
  def serialize(): String = {
    //TODO: implement
    ???
  }

  override def toString: String = try serialize() catch {
    case NonFatal(_) => "<non-serializable BinTree>"
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: BinTree if that eq this => true
    case that: BinTree if that.value == this.value => this.left == that.left && this.right == that.right
    case _ => false
  }
}

object BinTree {

  class SerializationException(msg: String = "Serialization failed", cause: Throwable = null) extends RuntimeException(msg, cause)

  def apply(value: String, left: BinTree = null, right: BinTree = null): BinTree = {
    new BinTree(value, Option(left), Option(right))
  }

  @throws[SerializationException]
  def deserialize(src: String): BinTree = {
    //TODO: implement
    ???
  }
}
