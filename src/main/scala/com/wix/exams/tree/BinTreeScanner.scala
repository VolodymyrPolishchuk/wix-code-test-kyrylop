package com.wix.exams.tree

import java.io.StringReader

import com.wix.exams.tree.BinTree.SerializationException

class BinTreeScanner(src: String) {
  val reader = new StringReader(src)

  def nextToken: Option[Token] = {
    reader.read().toChar match {
      case -1 => None
      case 'n' => Some(Null)
      case 'v' => Some(Value(readValue(readValueLength)))
      case _ => throw new SerializationException
    }
  }

  private def readValueLength: Int = {
    val lengthStr = new StringBuilder
    while (true) {
      reader.read().toChar match {
        case ':' => return lengthStr.toInt
        case ch if Character.isDigit(ch) => lengthStr.append(ch)
        case _ => throw new SerializationException
      }
    }
    throw new SerializationException
  }

  private def readValue(length: Int): String = {
    val cbuf = new Array[Char](length)
    reader.read(cbuf)
    String.valueOf(cbuf)
  }
}

sealed trait Token
case object Null extends Token
case class Value(value: String) extends Token