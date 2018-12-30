package org.asarkar.data

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class UndirectedGraphSpec extends FlatSpec {
  "UndirectedGraph" should "return vertices and edges" in {
    val g = Graph.undirected[Int, UndirectedEdge[Int]]
      .addEdge(UndirectedEdge(1, 2))
      .addEdge(UndirectedEdge(1, 4))
      .addEdge(UndirectedEdge(2, 3))
      .addEdge(UndirectedEdge(1, 2))
      .addEdge(UndirectedEdge(2, 1))
    g.vertices should contain allOf(1, 2, 3, 4)
    g.edges should contain theSameElementsAs Vector(
      UndirectedEdge(1, 2), UndirectedEdge(1, 4), UndirectedEdge(2, 3)
    )
  }

  it should "detect edges" in {
    val g = Graph.undirected[Int, UndirectedEdge[Int]]
      .addEdge(UndirectedEdge(1, 2))
      .addEdge(UndirectedEdge(2, 3))
    g.hasEdge(1, 2) shouldBe true
    g.hasEdge(2, 1) shouldBe true
    g.hasEdge(2, 3) shouldBe true
    g.hasEdge(3, 2) shouldBe true
    g.hasEdge(3, 1) shouldBe false
    g.hasEdge(1, 3) shouldBe false
  }

  it should "return outgoing edges" in {
    val g = Graph.undirected[Int, UndirectedEdge[Int]]
      .addEdge(UndirectedEdge(1, 2))
      .addEdge(UndirectedEdge(1, 3))

    g.outgoing(1) should contain theSameElementsAs Vector(UndirectedEdge(1, 2), UndirectedEdge(1, 3))
  }
}
