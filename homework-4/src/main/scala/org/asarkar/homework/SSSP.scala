package org.asarkar.homework

import org.asarkar.data.mutable.MinHeap
import org.jgrapht.Graph
import org.jgrapht.graph.DefaultWeightedEdge
import org.jgrapht.util.{FibonacciHeap, FibonacciHeapNode}

import scala.collection.JavaConverters._
import scala.util.Try

sealed abstract class SSSP(g: Graph[Int, DefaultWeightedEdge], s: Int) {
  def min: Double = dist.zipWithIndex
    .filterNot(_._2 == s)
    .map(_._1)
    .reduceOption[Double](math.min)
    .getOrElse(Double.NegativeInfinity)

  def hasNegativeCycle: Boolean

  def distTo(t: Int): Double = {
    if (hasPathTo(t))
      dist(t)
    else if (hasNegativeCycle)
      Double.NegativeInfinity
    else
      Double.PositiveInfinity
  }

  def hasPathTo(t: Int): Boolean = !hasNegativeCycle && dist(t) < Double.PositiveInfinity

  protected[homework] def dist: Array[Double]
}

// This implementation using Fibonacci heap is slower than my implementation. Why?
private sealed class DijkstraFibonacci(g: Graph[Int, DefaultWeightedEdge], s: Int) extends SSSP(g, s) {
  override protected[homework] val dist: Array[Double] = {
    val h = new FibonacciHeap[Int]
    val vertices = g.vertexSet
      .asScala
    val heapNodes = Array.ofDim[FibonacciHeapNode[Int]](vertices.size + 1)
    vertices
      .foreach { u =>
        heapNodes(u) = new FibonacciHeapNode(u)
        h.insert(heapNodes(u), if (u == s) 0d else Double.PositiveInfinity)
      }
    val a = Array.fill(vertices.size + 1)(Double.PositiveInfinity)

    Iterator.continually(if (h.isEmpty) None else Some(h.removeMin()))
      .takeWhile(_.isDefined)
      .flatten
      .filter(min => g.containsVertex(min.getData))
      .foreach { min =>
        val (dist, u) = (min.getKey, min.getData)
        heapNodes(u) = null
        a(u) = dist

        g.outgoingEdgesOf(u)
          .asScala
          .withFilter(e => heapNodes(g.getEdgeTarget(e)) != null)
          .foreach { e =>
            val v = heapNodes(g.getEdgeTarget(e))
            val w = dist + g.getEdgeWeight(e)
            Try(h.decreaseKey(v, w))
          }
      }
    a
  }

  override def hasNegativeCycle: Boolean = ???
}

private sealed class Dijkstra(g: Graph[Int, DefaultWeightedEdge], s: Int) extends SSSP(g, s) {
  override protected[homework] val dist: Array[Double] = {
    val vertices = g.vertexSet
    val h = MinHeap[Double, Int](vertices.asScala.map(v => (Double.PositiveInfinity, v)).toSeq)
    h.decreaseKey(0.0d, s)

    Iterator.continually(h.extractMin())
      .takeWhile(_.isDefined)
      .flatten
      .map { min =>
        val (dist, v) = min

        g.outgoingEdgesOf(v)
          .asScala
          .filter(e => h.contains(g.getEdgeTarget(e)))
          .foreach(e => h.decreaseKey(dist + g.getEdgeWeight(e), g.getEdgeTarget(e)))

        min
      }
      .foldLeft(Array.fill(vertices.size + 1)(Double.PositiveInfinity)) { (a, x) => a(x._2) = x._1; a }
  }

  override def hasNegativeCycle: Boolean = ???
}

private sealed class BellmanFord(g: Graph[Int, DefaultWeightedEdge], s: Int) extends SSSP(g, s) {
  override protected[homework] val dist: Array[Double] = {
    val vertices = g.vertexSet.asScala
    val n = vertices.size
    val a = Array.fill(n + 1, n + 1)(Double.PositiveInfinity)
    a(0)(s) = 0.0d

    for (i <- 1 to n)
      for (v <- vertices) {
        a(i)(v) = math.min(
          a(i - 1)(v),
          g.incomingEdgesOf(v)
            .asScala
            .map(e => a(i - 1)(g.getEdgeSource(e)) + g.getEdgeWeight(e))
            .reduceOption[Double](math.min)
            .getOrElse(Double.PositiveInfinity)
        )
      }

    if (a(n).sameElements(a(n - 1))) a(n) else Array.empty[Double]
  }

  override def hasNegativeCycle: Boolean = dist.isEmpty
}

object SSSP {
  def bellmanFord(g: Graph[Int, DefaultWeightedEdge], s: Int): SSSP = new BellmanFord(g, s)

  def dijkstra(g: Graph[Int, DefaultWeightedEdge], s: Int): SSSP = new Dijkstra(g, s)
}
