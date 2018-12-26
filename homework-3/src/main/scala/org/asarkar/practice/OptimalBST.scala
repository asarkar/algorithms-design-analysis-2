package org.asarkar.practice

object OptimalBST {
  def cost(items: IndexedSeq[Double]): Double = {
    val n = items.size
    val c = Array.fill[Double](n, n)(Double.NegativeInfinity)

    def cost(i: Int, j: Int): Double = {
      if (i > j) 0.0d // not defined
      else if (c(i)(j) >= 0) c(i)(j) // already computed
      else if (i == j) items(i) // single element BST
      // recursively compute the cost for the left and right subtrees
      else items.slice(i, j + 1).sum + (for (k <- i to j) yield cost(i, k - 1) + cost(k + 1, j)).min
    }

    for (i <- 0 until n)
      for (j <- i until n)
        c(i)(j) = cost(i, j)

    c.head.last
  }
}
