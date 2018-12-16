package org.asarkar.data.mutable

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class Heap[K: Ordering, V] protected(_items: Seq[(K, V)]) {
  protected val indexMap: mutable.Map[V, Int] = collection.mutable.Map.empty

  protected val items: ArrayBuffer[(K, V)] = ArrayBuffer.empty

  protected val first = 0

  def isEmpty: Boolean = items.isEmpty

  def contains(v: V): Boolean = indexMap.contains(v)

  def size: Int = items.size

  def peek: Option[(K, V)] = items.lift(first)

  override def toString: String = items.toString()

  protected def last: Int = items.size - 1

  protected def left(i: Int): Int = (i << 1) + 1

  protected def parent(i: Int): Int = math.floor((i - 1) >> 1).intValue()

  protected def sinkDownCondition: (K, K) => Boolean

  protected def swimUpCondition: (K, K) => Boolean

  _items // O(n)
    .foreach { e =>
    items += e
    indexMap(e._2) = last
  }

  (lastParentIndex to first by -1) // O(n/2)
    .foreach(sinkDown)

  /*
   * I figured out by drawing trees :) that:
   * LEFT(i) = 2i + 1, RIGHT(i) = 2i + 2, PARENT(i) = ⌊(i - 1)/2⌋
   * Given n elements, last parent is at index ⌊n/2 - 1⌋, first child at n - 1
   */
  protected def lastParentIndex: Int = math.floor((items.size >> 1) - 1).intValue()

  // O(1)
  protected def swap(i: Int, j: Int): Unit = {
    indexMap(items(i)._2) = j
    indexMap(items(j)._2) = i

    val tmp = items(i)
    items(i) = items(j)
    items(j) = tmp
  }

  protected def sinkDown(i: Int): Unit = {
    val l = left(i)
    val r = l + 1

    (for (j <- Seq(l, r) if compare(i, j, sinkDownCondition))
      yield j) match {
      case Seq() =>
      case xs =>
        /*
         * For a min-heap, sinkDownCondition = greater-than; so we want to pick the smaller of the two children.
         * For a max-heap, sinkDownCondition = less-than; so we want to pick the greater of the two children.
         * This should be intuitive; we are trying to avoid further sinkDown operations.
         */
        val child = xs.reduce((x, y) => if (compare(x, y, sinkDownCondition)) y else x)
        // the swap may violate the heaps rooted at the child and at the parent
        swap(i, child) // O(1)
        sinkDown(child)
        sinkDown(parent(i))
    }
  }

  // O(log n)
  protected def swimUp(i: Int): Unit = {
    val p = parent(i)

    if (compare(i, p, swimUpCondition)) {
      swap(i, p) // O(1)
      swimUp(p)
    }
  }

  protected def compare(i: Int, j: Int, f: (K, K) => Boolean): Boolean = {
    items.lift(i).zip(items.lift(j))
      .map(x => (x._1._1, x._2._1))
      .exists(f.tupled)
  }
}
