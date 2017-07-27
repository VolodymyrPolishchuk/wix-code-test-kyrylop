package com.wix.exams.tree

object BinTreeSerializer {

  def apply(tree: Option[BinTree]): String = {
    tree match {
      case None => "n"
      case Some(node) => s"v${this(node.value)}${this(node.left)}${this(node.right)}"
    }
  }

  private def apply(value: String) = s"${value.length}:$value"
}
