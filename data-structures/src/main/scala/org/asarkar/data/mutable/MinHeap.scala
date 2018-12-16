package org.asarkar.data.mutable

import scala.math.Ordering.Implicits._

class MinHeap[K: Ordering, V] private(_items: Seq[(K, V)]) extends Heap[K, V](_items) {
  // O(log n)
  def insert(k: K, v: V): Boolean = {
    if (this.contains(v)) false
    else {
      items += ((k, v)) // O(1)
      indexMap(v) = last // O(1)

      sinkDown(parent(last))
      true
    }
  }

  // O(log n)
  def extractMin(): Option[(K, V)] = {
    if (isEmpty) None
    else {
      swap(first, last) // O(1)
      val min = items.remove(last)
      indexMap -= min._2
      sinkDown(first)

      Some(min)
    }
  }

  def decreaseKey(k: K, v: V): Option[K] = {
    val old = for {
      i <- indexMap.get(v)
      j <- items.lift(i) if k < j._1
    } yield (i, j._1)

    old.foreach { o =>
      items(o._1) = (k, v) // O(1)
      // if key is decreased, heap rooted at i is not violated, but it's possible that the parent(i) is now > i
      swimUp(o._1)
    }

    old.map(_._2)
  }

  override protected def sinkDownCondition: (K, K) => Boolean =
    implicitly[Ordering[K]].gt

  override protected def swimUpCondition: (K, K) => Boolean =
    implicitly[Ordering[K]].lt
}

object MinHeap {
  def apply[K: Ordering, V](items: Seq[(K, V)]): MinHeap[K, V] = new MinHeap[K, V](items)

  def empty[K: Ordering, V]: MinHeap[K, V] = new MinHeap[K, V](Seq.empty[(K, V)])
}
