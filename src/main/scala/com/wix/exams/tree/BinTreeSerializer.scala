package com.wix.exams.tree

import com.wix.exams.tree.BinTree.SerializationException

class BinTreeSerializer {

  var handled: List[BinTree] = List[BinTree]()

  def apply(tree: Option[BinTree]): String = {
    tree match {
      case None => "n"
      case Some(node) => {
        if (handled.exists(n => n eq node)) {
          throw new SerializationException
        }
        handled = node :: handled
        s"v${this(node.value)}${this(node.left)}${this(node.right)}e"
      }
    }
  }

  private def apply(value: String) = s"${value.length}:$value"
}
