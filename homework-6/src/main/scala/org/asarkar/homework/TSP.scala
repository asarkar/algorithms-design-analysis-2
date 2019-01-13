package org.asarkar.homework

import java.awt.geom.Point2D

import scala.annotation.tailrec
import scala.collection.mutable

/*
 * In this assignment we will revisit an old friend, the traveling salesman problem (TSP).
 * This week you will implement a heuristic for the TSP, rather than an exact algorithm,
 * and as a result will be able to handle much larger problem sizes. Here is a data file
 * describing a TSP instance (original source: http://www.math.uwaterloo.ca/tsp/world/bm33708.tsp).
 *
 * The first line indicates the number of cities. Each city is a point in the plane,
 * and each subsequent line indicates the x- and y-coordinates of a single city.
 * The distance between two cities is defined as the Euclidean distance.
 *
 * You should implement the nearest neighbor heuristic:
 *
 * 1. Start the tour at the first city.
 * 2. Repeatedly visit the closest city that the tour hasn't visited yet. In case of a tie,
 * go to the closest city with the lowest index. For example, if both the third and fifth cities
 * have the same distance from the first city (and are closer than any other city), then the tour
 * should begin by going from the first city to the third city.
 * 3. Once every city has been visited exactly once, return to the first city to complete the tour.
 *
 * In the box below, enter the cost of the traveling salesman tour computed by the nearest neighbor
 * heuristic for this instance, rounded down to the nearest integer.
 *
 * [Hint: when constructing the tour, you might find it simpler to work with squared Euclidean distances
 * (i.e., the formula above but without the square root) than Euclidean distances.
 * But don't forget to report the length of the tour in terms of standard Euclidean distance.]
 *
 * ANSWER: The logic is fairly simple, but the size of the problem makes things difficult. This
 * implementation makes the following optimizations:
 * 1. If the input is sorted by x-coordinates, it stops looking for a nearest neighbor once
 * the square of the difference between two x-coordinates is greater than the
 * minimum distance already found. This optimization follows from the formula for the Euclidean distance.
 *
 * 2. It uses a BitSet to track the vertices already visited.
 *
 * BitSet is implemented based on an array of Longs.
 * The size of the array depends only on the highest number stored in it. Divide the highest number stored in it
 * by 64, rounding up, and we have the size of the array. Each element in the array consumes 8 bytes.
 * That means that dividing by 8 the greatest number stored in it, roughly yields the size in bytes of the BitSet.
 * "Roughly" because of virtual machine memory management overheads, because the pointer to the array also needs
 * some memory and because the array itself has some overhead.
 *
 * Since the largest number in the assignment file is 33708, BitSet occupies < 4.5 KB, while a Set[Double] would
 * take ≅ 270 KB.
 *
 * 3. cost(i, j) = cost(j, i), and for j < i, the latter has been already computed before.
 * But saving those requires additional memory (33708^2 ≅ 11 billion Doubles), so the costs are
 * computed every time.
 *
 * 4. The code works with the squared Euclidean distances, and avoids the square root operation
 * until it is necessary.
 *
 * For Euclidean TSP, Nearest Neighbor may produce a cycle log n times worse than the optimal one.
 * One option is to run the algorithm once on each vertex as the starting vertex, and take the best result.
 */
object TSP {
  def nearestNeighbor(points: IndexedSeq[Point2D.Double], sorted: Boolean): Double = {
    val visited = mutable.BitSet.empty // Array[Boolean] works too

    def nearest(i: Int) = {
      visited += i

      val p1 = points(i)
      points
        .iterator
        .zipWithIndex
        .scanLeft((-1, Double.PositiveInfinity, true)) { case ((k, minSq, _), (p2, j)) =>
          if (sorted && math.pow(p2.x - p1.x, 2) > minSq) (k, minSq, false)
          else {
            val last = j < points.size - 1

            if (visited.contains(j)) (k, minSq, last)
            else {
              val cost = p1.distanceSq(p2)

              // if cost = minSq, break the tie by returning the lower index
              if (cost < minSq) (j, cost, last)
              else (k, minSq, last)
            }
          }
        }
        .dropWhile(_._3)
        .take(1)
        .map(y => (y._1, math.sqrt(y._2)))
        .reduceOption((y, _) => y)
        .getOrElse((-1, Double.PositiveInfinity))
    }

    @tailrec
    def loop(i: Int, d: Double): Double = {
      nearest(i) match {
        case (k, m) if k >= 0 => loop(k, d + m)
        case _ => d + points(i).distance(points.head)
      }
    }

    loop(0, 0d)
  }
}
