package org.asarkar.data

import scala.collection.mutable.{HashMap => MutableMap, MultiMap => MutableMultiMap, Set => MutableSet}

sealed trait Edge[T] {
  def tail: T

  def head: T
}

sealed trait WeightedEdge[T] extends Edge[T] {
  def weight: Double
}

sealed abstract class AbstractUndirectedEdge[T] extends Edge[T] {
  def other(v: T): Option[T] = if (v == tail) Some(head) else if (v == head) Some(tail) else None

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: UndirectedEdge[T] => this.hashCode == that.hashCode
      case _ => false
    }
  }

  override def hashCode(): Int = {
    Seq(head, tail)
      .map(_.hashCode)
      .sorted
      .foldLeft(7)((result, hc) => 31 * result + hc)
  }
}

case class UndirectedEdge[T](override val tail: T, override val head: T) extends AbstractUndirectedEdge[T]

case class UndirectedWeightedEdge[T](override val tail: T, override val head: T, override val weight: Double)
  extends AbstractUndirectedEdge[T] with WeightedEdge[T]

case class DirectedEdge[T](override val tail: T, override val head: T) extends Edge[T]

case class DirectedWeightedEdge[T](override val tail: T, override val head: T, override val weight: Double)
  extends WeightedEdge[T]

sealed trait Graph[V, E <: Edge[V]] {
  def vertices: Iterable[V]

  def outgoing(v: V): Iterable[E]

  def edges: Iterable[E] = vertices
    .flatMap(outgoing)
    .toSet

  def hasEdge(v: V, w: V): Boolean = {
    neighbors(v).exists(_ == w)
  }

  def neighbors(v: V): Iterable[V] = {
    outgoing(v)
      .map(e => if (e.head == v) e.tail else e.head)
  }

  def contains(v: V): Boolean
}

sealed abstract class MutableGraph[V, E <: Edge[V]] extends Graph[V, E] {
  protected val _vertices = new MutableMap[V, MutableSet[E]] with MutableMultiMap[V, E]

  def addEdge(e: E): Unit = {
    _vertices.addBinding(e.tail, e)
  }

  override def vertices: Iterable[V] = _vertices.keys

  override def outgoing(v: V): Iterable[E] = Option.option2Iterable(_vertices.get(v))
    .flatten

  override def contains(v: V): Boolean = _vertices.contains(v)
}

sealed private class UndirectedGraph[V, E <: AbstractUndirectedEdge[V]] private[data]() extends MutableGraph[V, E] {
  override def addEdge(e: E): Unit = {
    super.addEdge(e)
    val reverse: AbstractUndirectedEdge[V] = e match {
      case x if e.isInstanceOf[UndirectedWeightedEdge[V]] => x.asInstanceOf[UndirectedWeightedEdge[V]]
        .copy(head = x.tail, tail = x.head)
      case _ => UndirectedEdge(e.head, e.tail)
    }
    super.addEdge(reverse.asInstanceOf[E])
  }
}

sealed private class DirectedGraph[V, E <: DirectedEdge[V]] private[data]() extends MutableGraph[V, E] {
  override def vertices: Iterable[V] = _vertices
    .flatMap(_._2.map(_.head).toSet)
}

sealed abstract class GraphBuilder[V, E <: Edge[V]] {
  protected[data] val graph: MutableGraph[V, E]

  def addEdge(e: E): this.type = {
    graph.addEdge(e)
    this
  }

  def build(): Graph[V, E] = graph
}

sealed class UndirectedGraphBuilder[V, E <: AbstractUndirectedEdge[V]] private[data]() extends GraphBuilder[V, E] {
  override protected[data] val graph: MutableGraph[V, E] = new UndirectedGraph[V, E]()
}

sealed class DirectedGraphBuilder[V, E <: DirectedEdge[V]] private[data]() extends GraphBuilder[V, E] {
  override protected[data] val graph: MutableGraph[V, E] = new DirectedGraph[V, E]
}

object Graph {
  def undirectedBuilder[V, E <: AbstractUndirectedEdge[V]]() = new UndirectedGraphBuilder[V, E]()

  def directedBuilder[V, E <: DirectedEdge[V]]() = new DirectedGraphBuilder[V, E]()
}


