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
        case None => stack match {
          case Nil => throw new SerializationException
          case node :: Nil => Some(node)
          case _ => throw new SerializationException
        }
        case Some(Null) => stack match {
          case Nil => stack = None :: stack
          case None :: _ => throw new SerializationException
          case Some(node) :: _ => setChild(node, None)
        }
        case Some(Value(value)) => stack match {
          case Nil => stack = Some(BinTree(value)) :: stack
          case None :: _ => throw new SerializationException
          case Some(node) :: _ => {
            val child = Some(BinTree(value))
            setChild(node, child)
            stack = child :: stack
          }
        }
      }
    }
  }

  private def setChild(node: BinTree, child: Option[BinTree]) = if (node.left == null) node.left = child else node.right = child

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
            throw new ScanningException(s"Unexpected char: $ch, possible chars: ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']")
          }
        }
        throw new ScanningException
      case 'e' => Some(End)
      case ch => throw new ScanningException(s"Unexpected char: $ch, possible chars: ['n', 'v', 'e']")
    }
  }

  private sealed trait Token
  private case object Null extends Token
  private case class Value(value: String) extends Token
  private case object End extends Token

  class ScanningException(msg: String = "Scanning failed", cause: Throwable = null) extends RuntimeException(msg, cause)
}
