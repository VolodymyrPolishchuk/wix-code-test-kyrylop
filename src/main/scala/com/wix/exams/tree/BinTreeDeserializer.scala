package com.wix.exams.tree

import java.io.{Reader, StringReader}

object BinTreeDeserializer {

  def apply(src: String): BinTree = {
    val reader = new StringReader(src)


  }

  private def nextToken(reader: Reader): Option[Token] = {
    val ch = reader.read()
    ch match {
      case 'n' => Some(Null)
      case 'v' => {

        val length = reader.readInt()
      }
    }
  }

  private sealed trait Token
  private case object Null extends Token
  private case class Value(value: String) extends Token
  private case object End extends Token
}
