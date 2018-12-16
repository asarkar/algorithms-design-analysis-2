package org.asarkar.data

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class DirectedGraphSpec extends FlatSpec {
  "DirectedGraph" should "return vertices and edges" in {
    val g = Graph.directedBuilder[Int, DirectedEdge[Int]]()
      .addEdge(DirectedEdge(1, 2))
      .addEdge(DirectedEdge(1, 4))
      .addEdge(DirectedEdge(2, 3))
      .addEdge(DirectedEdge(1, 2))
      .addEdge(DirectedEdge(2, 1))
      .build()
    g.vertices should contain allOf(1, 2, 3, 4)
    g.edges should contain theSameElementsAs Vector(
      DirectedEdge(1, 2), DirectedEdge(1, 4), DirectedEdge(2, 1), DirectedEdge(2, 3)
    )
  }

  it should "detect edges" in {
    val g = Graph.directedBuilder[Int, DirectedEdge[Int]]()
      .addEdge(DirectedEdge(1, 2))
      .addEdge(DirectedEdge(2, 3))
      .build()
    g.hasEdge(1, 2) shouldBe true
    g.hasEdge(2, 1) shouldBe false
    g.hasEdge(2, 3) shouldBe true
    g.hasEdge(3, 2) shouldBe false
  }

  it should "return outgoing edges" in {
    val g = Graph.directedBuilder[Int, DirectedEdge[Int]]()
      .addEdge(DirectedEdge(1, 2))
      .addEdge(DirectedEdge(1, 3))
      .build()

    g.outgoing(1) should contain theSameElementsAs Vector(DirectedEdge(1, 2), DirectedEdge(1, 3))
  }

  it should "return neighboring vertices" in {
    val g = Graph.directedBuilder[Int, DirectedEdge[Int]]()
      .addEdge(DirectedEdge(1, 2))
      .addEdge(DirectedEdge(1, 3))
      .build()

    g.neighbors(1) should contain allOf(2, 3)
  }
}
