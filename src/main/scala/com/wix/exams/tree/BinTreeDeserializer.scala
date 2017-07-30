package com.wix.exams.tree

import com.wix.exams.tree.BinTree.SerializationException
import com.wix.exams.tree.BinTreeDeserializer.Position.{Left, Position, Right}

object BinTreeDeserializer {

  def apply(src: String): Option[BinTree] = {
    val scanner = new BinTreeScanner(src)
    def apply0(stack: List[(BinTree, Position)]): Unit = stack match {
      case Nil =>
      case (parent, position) :: tail => scanner.nextToken match {
        case None => throw new SerializationException
        case Some(Null) => apply0(tail)
        case Some(Value(value)) =>
          val child = BinTree(value)
          position match {
            case Left => parent.left = Some(child)
            case Right => parent.right = Some(child)
          }
          apply0((child, Left) :: (child, Right) :: tail)
      }
    }
    val sentinel = BinTree("")
    apply0(List((sentinel, Left)))
    sentinel.left
  }

  object Position extends Enumeration {
    type Position = Value
    val Left, Right = Value
  }
}