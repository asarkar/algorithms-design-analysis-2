package org.asarkar.practice

import scala.annotation.tailrec
import scala.collection.mutable

class Huffman(weights: Seq[Double]) {

  private case class Node(weight: Double, left: Option[Node], right: Option[Node]) {
    def isLeaf: Boolean = left.isEmpty && right.isEmpty
  }

  @tailrec
  private def build(pq: mutable.PriorityQueue[Node]): Node = {
    val x = pq.dequeue()
    if (pq.isEmpty) x
    else {
      val y = pq.dequeue()
      pq += Node(x.weight + y.weight, Some(x), Some(y))
      build(pq)
    }
  }

  private def enc(
                   node: Node,
                   prefix: String = "",
                   map: mutable.Map[String, Double] = mutable.Map.empty[String, Double]
                 ): mutable.Map[String, Double] = {
    if (node.isLeaf) map += prefix.toString -> node.weight
    else {
      Seq(
        node.left.map(enc(_, prefix + "0", map)),
        node.right.map(enc(_, prefix + "1", map))
      )
        .flatten
        .reduce(_ ++= _)
    }
  }

  val enc: Map[String, Double] = {
    val pq = collection.mutable.PriorityQueue
      .newBuilder[Node](Ordering.by[Node, Double](_.weight).reverse)
    weights
      .foreach(w => pq += Node(w, None, None))

    enc(build(pq.result())).toMap
  }

  val avg: Double = enc
    .map(x => x._1.length * x._2)
    .sum
}
