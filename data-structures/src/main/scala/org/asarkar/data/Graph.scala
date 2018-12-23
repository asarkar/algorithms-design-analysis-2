package org.asarkar.data

sealed trait Edge[T] {
  def tail: T

  def head: T
}

sealed trait WeightedEdge[T] extends Edge[T] {
  def weight: Double
}

sealed protected[data] abstract class AbstractUndirectedEdge[T] extends Edge[T] {
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
  type G <: Graph[V, E]

  def vertices: Iterable[V] = _vertices.keys ++
    _vertices
      .flatMap(_._2.map(_.head).toSet)

  def outgoing(v: V): Iterable[E] = Option.option2Iterable(_vertices.get(v))
    .flatten

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

  def contains(v: V): Boolean = _vertices.contains(v)

  def addEdge(e: E): G

  protected[data] def _vertices: Map[V, Set[E]]

  protected[data] def addEdge(e: E*): Map[V, Set[E]] = {
    e.foldLeft(_vertices) { (map, x) =>
      val heads = map.get(x.tail) match {
        case Some(xs) => xs
        case _ => Set.empty[E]
      }
      map + (x.tail -> (heads + x))
    }
  }
}

sealed private[data] class UndirectedGraph[V, E <: AbstractUndirectedEdge[V]](
                                                                               override protected[data] val _vertices: Map[V, Set[E]] =
                                                                               Map.empty[V, Set[E]]
                                                                             ) extends Graph[V, E] {
  override type G = UndirectedGraph[V, E]

  override def addEdge(e: E): G = {
    val reverse: AbstractUndirectedEdge[V] = e match {
      case x: UndirectedWeightedEdge[V] => x.copy(head = x.tail, tail = x.head)
      case _ => UndirectedEdge(e.head, e.tail)
    }
    new UndirectedGraph(super.addEdge(e, reverse.asInstanceOf[E]))
  }
}

sealed private[data] class DirectedGraph[V, E <: DirectedEdge[V]](
                                                                   override protected[data] val _vertices: Map[V, Set[E]] =
                                                                   Map.empty[V, Set[E]]
                                                                 ) extends Graph[V, E] {
  override type G = DirectedGraph[V, E]

  override def addEdge(e: E): G = new DirectedGraph(super.addEdge(Seq(e): _*))
}

object Graph {
  def undirected[V, E <: AbstractUndirectedEdge[V]] = new UndirectedGraph[V, E]()

  def directed[V, E <: DirectedEdge[V]] = new DirectedGraph[V, E]()
}


