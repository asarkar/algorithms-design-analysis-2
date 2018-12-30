package org.asarkar.homework

import org.jgrapht.graph.{DefaultWeightedEdge, DirectedWeightedPseudograph}
import org.jgrapht.{Graph, Graphs}

import scala.collection.JavaConverters._

/*
 * In this assignment you will implement one or more algorithms for the all-pairs shortest-path problem.
 * The first line indicates the number of vertices and edges, respectively. Each subsequent line describes an edge
 * (the first two numbers are its tail and head, respectively) and its length (the third number).
 * NOTE: some of the edge lengths are negative.
 * NOTE: These graphs may or may not have negative-cost cycles.
 *
 * Your task is to compute the shortest shortest path. Precisely, you must first identify which, if any, of the three
 * graphs have no negative cycles. For each such graph, you should compute all-pairs shortest paths and remember the
 * smallest one (i.e., compute min{d(u,v)}, where d(u,v) denotes the shortest-path distance from u to v).
 *
 * If each of the three graphs has a negative-cost cycle, then enter "NULL" in the box below. If exactly one graph has
 * no negative-cost cycles, then enter the length of its shortest shortest path in the box below. If two or more of the
 * graphs have no negative-cost cycles, then enter the smallest of the lengths of their shortest shortest paths in the
 * box below.
 *
 * OPTIONAL: You can use whatever algorithm you like to solve this question. If you have extra time, try comparing
 * the performance of different all-pairs shortest-path algorithms!
 *
 * ANSWER: I implemented both Floyd-Warshall and Johnson's algorithms. FW takes O(n^3) time (evident from the three
 * for-loops). Johnson's takes O(mn log(n)), which is better than FW running time for sparse graphs (m = O(n)).
 * However, Johnson's algorithm requires mutating the input graph, which is not a good programming practice. My
 * implementation makes a mutable clone of the input graph, and performs all modifications on the clone. It also
 * uses a graph library that allows directly modifying the edge weight; most libraries don't.
 *
 * NOTE: If any of the shortest paths is negative, it's not necessary to run Johnson's algorithm in it's entirety for
 * the purpose of the assignment. We can simply return the minimum value in the array returned by the
 * Bellman-Ford algorithm. Let G' be the modified graph after adding the vertex s, d(u,v) <= 0 ∀ (u,v) in G'.
 * Since s is directly connected to both u and v, if there's an existing path p between u and v in G' s.t. length(p) > 0,
 * it is not the shortest path found by BF algorithm. In other words, running BF algorithm on the modified graph
 * upper bounds all shortest path lengths by zero, while preserving the negative path lengths.
 *
 * TODO:
 * 1. Copying the input graph incurs some overhead: profile the application to find out exactly how much.
 * 2. Both FW and Johnson's algorithms can be parallelized leading to quite good speedups.
 */
sealed abstract class APSP(g: Graph[Int, DefaultWeightedEdge]) {
  def min: Double = dist.zipWithIndex
    .flatMap(x => x._1
      .filterNot(_ == x._2)
    )
    .reduceOption[Double](math.min)
    .getOrElse(Double.NegativeInfinity)

  def minFrom(s: Int): Double = dist.lift(s)
    .map(_.zipWithIndex
      .filterNot(_._2 == s)
      .map(_._1)
      .min
    )
    .getOrElse(Double.NegativeInfinity)

  def hasPathBetween(s: Int, t: Int): Boolean = !hasNegativeCycle && distBetween(s, t) < Double.PositiveInfinity

  def hasNegativeCycle: Boolean = dist.isEmpty

  def distBetween(s: Int, t: Int): Double = dist(s)(t)

  protected def dist: Array[Array[Double]]
}

/*
 * Instead of using a 3-D array as shown in the lecture video, I use a 2-D array. CLRS exercise 25.2-4 asks to
 * prove it is correct. The proof can be seen here: https://walkccc.github.io/CLRS/Chap25/25.2/#252-4
 */
private sealed class FloydWarshall(g: Graph[Int, DefaultWeightedEdge]) extends APSP(g) {
  override protected def dist: Array[Array[Double]] = {
    val n = g.vertexSet.size
    val a = Array.tabulate(n + 1, n + 1) { (i, j) =>
      if (i == j) 0.0d
      else if (g.containsVertex(i) && g.containsVertex(j))
      // for parallel edges, take the one with the min weight
        g.getAllEdges(i, j)
          .asScala
          .map(g.getEdgeWeight)
          .reduceOption[Double](math.min)
          .getOrElse(Double.PositiveInfinity)
      else Double.PositiveInfinity
    }

    for (k <- 1 to n)
      for (i <- 1 to n)
        for (j <- 1 to n) {
          a(i)(j) = math.min(
            a(i)(j),
            a(i)(k) + a(k)(j)
          )
        }
    if (a.indices.exists(i => a(i)(i) < 0))
      Array.empty[Array[Double]]
    else a
  }
}

private sealed class Johnson(g: Graph[Int, DefaultWeightedEdge]) extends APSP(g) {
  override protected def dist: Array[Array[Double]] = {
    val vertices = g.vertexSet
    val n = vertices.size
    val s = if (g.containsVertex(0)) n else 0

    val gPrime = new DirectedWeightedPseudograph[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
    assert(Graphs.addGraph(gPrime, g), s"Failed to clone graph")

    assert(gPrime.addVertex(s), s"Failed to add vertex: $s")
    vertices
      .asScala
      .foreach(u => assert(Graphs.addEdge(gPrime, s, u, 0.0d) != null, s"Failed to add edge: $s -> $u"))

    val bf = SSSP.bellmanFord(gPrime, s)

    if (bf.hasNegativeCycle) {
      Array.empty[Array[Double]]
    } else {
      val p = bf.dist
      assert(gPrime.removeVertex(s), s"Failed to remove vertex: $s")
      gPrime.edgeSet
        .asScala
        .foreach { e =>
          val (u, v) = (gPrime.getEdgeSource(e), gPrime.getEdgeTarget(e))
          // w >=0 by Triangle Inequality
          val w = gPrime.getEdgeWeight(e) + p(u) - p(v)
          assert(w >= 0, s"Edge weight is negative for $u -> $v")
          gPrime.setEdgeWeight(e, w)
        }

      val a = Array.fill[Double](n + 1, n + 1)(Double.PositiveInfinity)
      vertices
        .asScala
        .foreach { u =>
          // JGraphT Dijkstra is slower
          val d = SSSP.dijkstra(gPrime, u).dist
          vertices.forEach(v => a(u)(v) = d(v) - p(u) + p(v))
        }
      a
    }
  }
}

object APSP {
  def floydWarshall(g: Graph[Int, DefaultWeightedEdge]): APSP = new FloydWarshall(g)

  def johnson(g: Graph[Int, DefaultWeightedEdge]): APSP = new Johnson(g)
}
