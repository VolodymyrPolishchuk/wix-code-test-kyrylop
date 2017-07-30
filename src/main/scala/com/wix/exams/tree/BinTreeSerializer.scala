package com.wix.exams.tree

import com.wix.exams.tree.BinTree.SerializationException

object BinTreeSerializer {

  def apply(tree: Option[BinTree]): String = {
    var handled = List[BinTree]()
    def apply0(acc: String, stack: List[Option[BinTree]]): String = stack match {
      case Nil => acc
      case None :: tail => apply0(s"${acc}n", tail)
      case Some(node) :: tail =>
        if (handled.exists(_ eq node)) {
          throw new SerializationException
        }
        handled = node :: handled
        apply0(s"${acc}v${node.value.length}:${node.value}", node.left :: node.right :: tail)
    }
    apply0("", List(tree))
  }
}