package org.asarkar.homework

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class SSSPSpec extends FlatSpec with TableDrivenPropertyChecks {
  "BellmanFord" should "compute the shortest paths or indicate negative cycle" in {
    val data =
    // format: off
      Table(("archive", "file", "src", "firstVertex", "min"),
        ("/sssp-test.zip", "sssp-test.txt", 0, 0, 2.0d),
        ("/tinyEWDn.zip", "tinyEWDn.txt", 0, 0, 0.26d),
        ("/tinyEWDn.zip", "tinyEWDn-negative.txt", 0, 0, Double.NegativeInfinity),
      )
    // format: on

    forAll(data) { (archive, file, src, firstVertex, min) =>
      val bf = SSSP.bellmanFord(fromFile(archive, file, firstVertex), src)
      bf.min shouldBe min
      bf.hasNegativeCycle shouldBe (min == Double.NegativeInfinity)
    }
  }

  "Dijkstra" should "compute the shortest paths" in {
    val dijkstra = SSSP.dijkstra(fromFile("/sssp-test.zip", "sssp-test.txt", 0), 0)
    a[NotImplementedError] should be thrownBy dijkstra.hasNegativeCycle
    dijkstra.min shouldBe (2.0d +- 0.01d)
  }
}
