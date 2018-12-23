package org.asarkar.data.mutable

class UnionFind private(val n: Int) {
  private val parent: Array[Int] = Array.iterate(0, n + 1)(_ + 1)
  // since n is an integer, the max value is 2^31 - 1 (msb bit for sign, and 1 subtracted for zero). Since the height
  // of the tree can be at max log₂(n), rank <= 31. we use the smallest available numeric data type to save space
  private val rank: Array[Byte] = Array.ofDim(n + 1)
  private var count = n

  def find(p: Int): Int = {
    if (p < 1 || p > n) throw new IndexOutOfBoundsException(s"Index: $p must be in the range: [1, ${n + 1}]")

    Iterator.iterate(parent(p)) { i =>
      // Link to grandparent; not as good as linking to the roor, but according to Prof. Sedgewick, almost as good
      // in all practical use cases
      if (!isRoot(i)) parent(i) = parent(parent(i))
      parent(i)
    }
      .dropWhile(!isRoot(_))
      .next()
  }

  def isRoot(p: Int): Boolean = p == parent(p)

  def size: Int = count

  def isConnected(p: Int, q: Int): Boolean = find(p) == find(q)

  def union(p: Int, q: Int): Int = {
    val rootP = find(p)
    val rootQ = find(q)

    if (rootP == rootQ) rootP
    else {
      val larger = if (rank(rootP) > rank(rootQ)) rootP else rootQ

      if (rank(rootP) < rank(rootQ)) parent(rootP) = rootQ
      else if (rank(rootP) > rank(rootP)) parent(rootQ) = rootP
      else {
        parent(rootQ) = rootP
        rank(rootP) = (rank(rootP) + 1).toByte
      }
      // merger of two components reduces the number of connected components by one
      count -= 1

      larger
    }
  }
}

object UnionFind {
  def apply(n: Int): UnionFind = new UnionFind(n)
}
