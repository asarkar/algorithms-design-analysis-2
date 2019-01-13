package org.asarkar.practice

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class HuffmanSpec extends FlatSpec {
  "Huffman" should "compute the average encoding length for the given symbols" in {
    val h = new Huffman(Seq(.28, .27, .2, .15, .1))

    h.avg shouldBe (2.25 +- 0.01d)
  }
}
