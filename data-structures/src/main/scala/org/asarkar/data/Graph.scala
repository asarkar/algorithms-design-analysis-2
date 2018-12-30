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
      case that: AbstractUndirectedEdge[T] => this.hashCode == that.hashCode
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
  extends AbstractUndirectedEdge[T] with WeightedEdge[T] {
  override def hashCode(): Int = 31 * super.hashCode() + weight.hashCode()
}

sealed protected[data] abstract class AbstractDirectedEdge[T] extends Edge[T]

case class DirectedEdge[T](override val tail: T, override val head: T) extends AbstractDirectedEdge[T]

case class DirectedWeightedEdge[T](override val tail: T, override val head: T, override val weight: Double)
  extends AbstractDirectedEdge[T] with WeightedEdge[T]

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
    outgoing(v)
      .map(e => if (e.head == v) e.tail else e.head)
      .exists(_ == w)
  }

  def contains(v: V): Boolean = _vertices.contains(v)

  def addEdge(e: E): G

  protected def _vertices: Map[V, Set[E]]

  protected def addEdges(map: Map[V, Set[E]], e: E*): Map[V, Set[E]] = {
    e.foldLeft(map) { (m, x) =>
      val heads = m.get(x.tail) match {
        case Some(xs) => xs
        case _ => Set.empty[E]
      }
      m + (x.tail -> (heads + x))
    }
  }
}

sealed class UndirectedGraph[V, E <: AbstractUndirectedEdge[V]](
                                                                 override protected[data] val _vertices: Map[V, Set[E]] =
                                                                 Map.empty[V, Set[E]]
                                                               ) extends Graph[V, E] {
  override type G = UndirectedGraph[V, E]

  override def addEdge(e: E): G = {
    val reverse: AbstractUndirectedEdge[V] = e match {
      case x: UndirectedWeightedEdge[V] => x.copy(head = x.tail, tail = x.head)
      case _ => UndirectedEdge(e.head, e.tail)
    }
    new UndirectedGraph(super.addEdges(_vertices, e, reverse.asInstanceOf[E]))
  }
}

sealed class DirectedGraph[V, E <: AbstractDirectedEdge[V]](
                                                             override protected[data] val _vertices: Map[V, Set[E]] =
                                                             Map.empty[V, Set[E]],
                                                             protected[data] val _reverseVertices: Map[V, Set[E]] =
                                                             Map.empty[V, Set[E]]
                                                           ) extends Graph[V, E] {
  override type G = DirectedGraph[V, E]

  override def addEdge(e: E): G = {
    val m1 = super.addEdges(_vertices, Seq(e): _*)
    val e1 = e match {
      case x: DirectedWeightedEdge[V] => x.copy(tail = x.head, head = x.tail)
      case _ => DirectedEdge(e.head, e.tail)
    }
    val m2 = super.addEdges(_reverseVertices, Seq(e1.asInstanceOf[E]): _*)
    new DirectedGraph(m1, m2)
  }

  def incoming(v: V): Iterable[E] = Option.option2Iterable(_reverseVertices.get(v))
    .flatten
    .map { e =>
      (e match {
        case x: DirectedWeightedEdge[V] => x.copy(tail = x.head, head = x.tail)
        case _ => DirectedEdge(e.head, e.tail)
      }).asInstanceOf[E]
    }

  def hasVertex(v: V): Boolean = _vertices.contains(v) || _reverseVertices.contains(v)
}

object Graph {
  def undirected[V, E <: AbstractUndirectedEdge[V]]: UndirectedGraph[V, E] = new UndirectedGraph[V, E]()

  def directed[V, E <: AbstractDirectedEdge[V]]: DirectedGraph[V, E] = new DirectedGraph[V, E]()
}


