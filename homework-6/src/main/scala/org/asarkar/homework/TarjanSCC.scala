package org.asarkar.homework

import org.asarkar.data.{DirectedEdge, DirectedGraph}

import scala.collection.mutable

/*
 * The DFS trees form a spanning forest of the graph. The roots of these trees are called the "roots" of the
 * strongly connected components.
 * Invariant: A vertex is not removed from the stack if one of its ancestors is on the stack.
 *
 * Each vertex has two attributes:
 * 1. id: Order in which the nodes are visited. Immutable once assigned.
 * 2. lowlink: The smallest index of any vertex reachable from it, including itself.
 * Initially same as id, but may get updated later during DFS.
 *
 * https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
 * https://www.youtube.com/watch?v=TyWtx7q2D7Y
 */
class TarjanSCC(g: DirectedGraph[Int, DirectedEdge[Int]]) {
  private val Undefined = -1

  private val vertices = g.vertices
  private val n = vertices.size
  // avoid resizing
  private val ids = new java.util.HashMap[Int, Int](n)
  private val lows = new java.util.HashMap[Int, Int](n)
  private val stack = mutable.ListBuffer.empty[Int]
  private val onStack = mutable.Set.empty[Int]
  private var id = 0

  vertices
    .withFilter(!ids.containsKey(_))
    .foreach(scc)

  private def scc(v: Int): Unit = {
    ids.put(v, id)
    lows.put(v, id)
    id += 1
    v +=: stack
    onStack += v

    g.outgoing(v)
      .map(_.head)
      .foreach { w =>
        // (v, w) is a back edge; w is v's ancestor and has lower id, update v's lowlink
        // if w is not on stack, and has already been visited, (v, w) is a cross-edge and is ignored
        if (onStack.contains(w))
          lows.put(v, math.min(lows.get(v), ids.get(w)))
        // (v, w) is a tree edge
        else if (!ids.containsKey(w)) {
          scc(w)
          lows.put(v, math.min(lows.get(v), lows.get(w)))
        }
      }

    // root of scc
    if (ids.get(v) == lows.get(v)) {
      Iterator
        .continually(stack.remove(0))
        .takeWhile(_ != v)
        .foreach(onStack -= _)
      onStack -= v
    }
  }

  def isConnected(v: Int, w: Int): Boolean = {
    lows.containsKey(v) &&
      lows.containsKey(w) &&
      lows.get(v) == lows.get(w)
  }
}
