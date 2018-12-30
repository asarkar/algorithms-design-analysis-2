package org.asarkar.homework

import org.jgrapht.Graph
import org.jgrapht.graph.DefaultWeightedEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class Assignment4Spec extends FlatSpec {
  "FloydWarshall" should "compute the shortest paths from the assignment files" ignore {
    min(APSP.floydWarshall) shouldBe (-19.0d +- 0.01d)
  }

  "Johnson" should "compute the shortest paths from the assignment files" ignore {
    min(APSP.johnson) shouldBe (-19.0d +- 0.01d)
  }

  private def min(f: Graph[Int, DefaultWeightedEdge] => APSP): Double = {
    Iterator("g1.txt", "g2.txt", "g3.txt")
      .map(file => fromFile("/g.zip", file, 1))
      .map(f)
      .map(_.min)
      .filter(_ > Double.NegativeInfinity)
      .reduceOption[Double](math.min)
      .getOrElse(Double.PositiveInfinity)
  }
}
