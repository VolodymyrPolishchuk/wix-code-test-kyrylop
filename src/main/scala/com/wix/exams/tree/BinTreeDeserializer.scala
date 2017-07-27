package com.wix.exams.tree

import java.io.{Reader, StringReader}

import com.wix.exams.tree.BinTree.SerializationException

object BinTreeDeserializer {

  def apply(src: String): Option[BinTree] = {
    val reader = new StringReader(src)
    var stack: List[Option[BinTree]] = Nil
    while (true) {
      val token = nextToken(reader)
      token match {
        case None => throw new SerializationException
        case Some(Null) => stack match {
          case Nil => return None
          case None :: _ => throw new SerializationException
          case Some(node) :: _ => setChild(node, None)
        }
        case Some(Value(value)) => stack match {
          case Nil => stack = Some(binTree(value)) :: stack
          case None :: _ => throw new SerializationException
          case Some(node) :: _ =>
            val child = Some(binTree(value))
            setChild(node, child)
            stack = child :: stack
        }
        case Some(End) => stack match {
          case Nil => throw new SerializationException
          case Some(node) :: tail =>
            if (tail.isEmpty){
              if (isReady(node)) {
                return Some(node)
              } else {
                throw new SerializationException
              }
            } else {
              stack = tail
            }
        }
      }
    }
    throw new SerializationException
  }

  private def binTree(value: String): BinTree = {
    val tree = BinTree(value)
    tree.left = null
    tree.right = null
    tree
  }

  private def setChild(node: BinTree, child: Option[BinTree]) =
    if (node.left == null) {
      node.left = child
    } else {
      node.right = child
    }

  private def isReady(node: BinTree) = node.left != null && node.right != null

  private def nextToken(reader: Reader): Option[Token] = {
    reader.read().toChar match {
      case -1 => None
      case 'n' => Some(Null)
      case 'v' =>
        val lengthStr = new StringBuilder
        var ch = 'a'
        while (true) {
          ch = reader.read().toChar
          if (ch == ':') {
            val length = lengthStr.toInt
            val cbuf = new Array[Char](length)
            reader.read(cbuf)
            return Some(Value(String.valueOf(cbuf)))
          } else if (Character.isDigit(ch)) {
            lengthStr.append(ch)
          } else {
            throw new SerializationException(s"Unexpected char: $ch, possible chars: ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']")
          }
        }
        throw new SerializationException
      case 'e' => Some(End)
      case ch => throw new SerializationException(s"Unexpected char: $ch, possible chars: ['n', 'v', 'e']")
    }
  }

  private sealed trait Token
  private case object Null extends Token
  private case class Value(value: String) extends Token
  private case object End extends Token
}
