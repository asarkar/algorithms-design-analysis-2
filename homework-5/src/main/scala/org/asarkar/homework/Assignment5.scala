package org.asarkar.homework

import java.util.{HashMap => JavaHashMap}

/* In this assignment you will implement one or more algorithms for the traveling salesman problem,
 * such as the dynamic programming algorithm covered in the video lectures.
 * Here is a data file describing a TSP instance.
 * tsp.txt
 *
 * The first line indicates the number of cities. Each city is a point in the plane, and each subsequent line indicates
 * and each subsequent line indicates the x- and y-coordinates of a single city. The distance between two cities is
 * defined as the Euclidean distance.
 *
 * In the box below, type in the minimum cost of a traveling salesman tour for this instance, rounded down
 * to the nearest integer.
 *
 * OPTIONAL: If you want bigger data sets to play with, check out the TSP instances from around the world.
 * The smallest data set (Western Sahara) has 29 cities, and most of the data sets are much bigger than that.
 * What's the largest of these data sets that you're able to solve --- using dynamic programming or, if you like,
 * a completely different method?
 *
 * HINT: You might experiment with ways to reduce the data set size. For example, trying plotting the points.
 * Can you infer any structure of the optimal solution? Can you use that structure to speed up your algorithm?
 */
object Assignment5 {

  implicit class SeqOps(private val xs: Seq[Int]) extends AnyVal {
    def deepHashCode: Int = xs match {
      case Seq() => 0
      case _ => xs.foldLeft(7)((result, x) => 31 * result + x.hashCode)
    }
  }

  def tsp(costs: Map[Int, Map[Int, Double]], s: Int): Double = {
    def cost(i: Int, j: Int): Double = costs
      .getOrElse(i, Map.empty[Int, Double])
      .getOrElse(j, costs(j)(i))

    val others = costs.keys
      .filterNot(_ == s)
      .toSeq

    // avoid resizing; a set of size n has 2^n subsets
    val a = new JavaHashMap[Int, JavaHashMap[Int, Double]](1 << others.size)
    /*
     * Consider s = 1 and a combination {2, 3, 4}. For j = 2, we want to calculate the cost to reach
     * 2 from 1 via vertices 3 and 4 (a({3, 4}, 2)). ys below represents the vertex set {3, 4}. The vertex
     * immediately preceding 2 could be 3 or 4. If it's 3, then the cost is w(2, 3) + whatever it takes to
     * reach 3 from 1 via 4 (a({4}, 3)). If the vertex immediately preceding 2 is 4, the cost is w(2, 4) +
     * a({3}, 4). We take the minimum of the two choices.
     */
    for (m <- 1 to others.size; xs <- others.combinations(m); j <- xs) {
      val ys = xs.filterNot(_ == j)
      val hc = ys.deepHashCode
      a.compute(hc, (_: Int, v: JavaHashMap[Int, Double]) => {
        val v1 = if (v == null) new JavaHashMap[Int, Double](others.size) else v
        v1.put(j, ys
          .map(k => a.get(ys.filterNot(_ == k).deepHashCode).get(k) + cost(k, j))
          .reduceOption[Double](math.min)
          .getOrElse(cost(s, j)))
        v1
      })
    }
    // cost of returning to 's' from any of the other vertices
    others
      .map(k => a.get(others.filterNot(_ == k).deepHashCode).get(k) + cost(k, s))
      .min
  }
}
