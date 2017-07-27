package com.wix.exams.tree

import com.wix.exams.tree.BinTree.SerializationException
import org.specs2.mutable.SpecWithJUnit

class BinTreeTest extends SpecWithJUnit {

  "handles trivial tree" >> {
    val tree = BinTree("A")
    val asString = tree.serialize()
    println(asString)

    BinTree.deserialize(asString) must be_===(tree)
  }

  "handles non-trivial tree" >> {
    val tree =
      BinTree("A",
        BinTree("B",
          BinTree("C",
            null,
            BinTree("E")),
          BinTree("P")),
        BinTree("G"))

    val asString = tree.serialize()
    println(asString)

    BinTree.deserialize(asString) must be_===(tree)
  }

  "fails on invalid deserialization input" >> {
    BinTree.deserialize("I'm not a tree!!!") must throwA[SerializationException]
  }

  "fails on cycle" >> {
    val node =
      BinTree("T",
        BinTree("W"),
        null)

    val tree =
      BinTree("A",
        BinTree("B",
          BinTree("C",
            null,
            BinTree("E")),
          BinTree("P",
            node,
            null)),
        BinTree("G"))

    node.right = Some(tree)

    tree.serialize() must throwA[SerializationException]
  }

}
