package org.asarkar.homework

import org.asarkar.data.{DirectedEdge, Graph}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TarjanSCCSpec extends FlatSpec {
  "TarjanSCC" should "compute the SCC in the given graph" in {
    // src/test/resources/scc.jpg
    val g = Seq(
      0 -> 1,
      1 -> 2,
      2 -> 0,
      3 -> 4,
      3 -> 7,
      4 -> 5,
      5 -> 0,
      5 -> 6,
      6 -> 0,
      6 -> 2,
      6 -> 4,
      7 -> 3,
      7 -> 5
    )
      .foldLeft(Graph.directed[Int, DirectedEdge[Int]])((graph, e) => graph.addEdge(DirectedEdge(e._1, e._2)))

    val scc = new TarjanSCC(g)
    Seq(
      Seq(0, 1, 2),
      Seq(4, 5, 6),
      Seq(3, 7)
    )
      .forall(_.combinations(2)
        .forall(ys => scc.isConnected(ys.head, ys.last))
      ) shouldBe true
  }
}
