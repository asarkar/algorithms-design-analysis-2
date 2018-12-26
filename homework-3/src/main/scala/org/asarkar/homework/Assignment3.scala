package org.asarkar.homework

import scala.annotation.tailrec
import scala.collection.{BitSet, mutable}

object Assignment3 {
  /*
   * 1. In this programming problem and the next you'll code up the knapsack algorithm from lecture.
   * Let's start with a warm-up. Download the text file below:
   * knapsack1.txt
   *
   * This file describes a knapsack instance, and it has the following format:
   * [knapsack_size][number_of_items]
   * [value_1] [weight_1]
   * [value_2] [weight_2]
   * ...
   * For example, the third line of the file is "50074 659", indicating that the second item has value 50074
   * and size 659, respectively.
   * You can assume that all numbers are positive. You should assume that item weights and the knapsack capacity
   * are integers.
   *
   * 2. This problem also asks you to solve a knapsack instance, but a much bigger one. The format of the file
   * is the same as before.
   *
   * This instance is so big that the straightforward iterative implemetation uses an infeasible amount of time
   * and space. So you will have to be creative to compute an optimal solution. One idea is to go back to a
   * recursive implementation, solving subproblems --- and, of course, caching the results to avoid redundant work
   * --- only on an "as needed" basis. Also, be sure to think about appropriate data structures for storing and
   * looking up solutions to subproblems.
   *
   * ANSWER: We make some improvements over the algorithm shown in the lecture. First of all, we only ever use
   * the 1-D array corresponding to the last and current values of i, so an array of size n + 1 for i is a waste;
   * we use an array of size two.
   * Second of all, there is no need to loop over values of j less than the current weight, we simply copy over
   * the relevant portion of the array from the last value of i.
   * To determine current and previous values of i, we use modular arithmetic along with logic XOR.
   */
  def knapsack(size: Int, items: IndexedSeq[(Int, Int)]): Int = {
    val a = Array.ofDim[Int](2, size + 1)
    for (i <- 1 to items.size) {
      val current = i % 2
      val previous = current ^ 1
      val (v, w) = items(i - 1)

      Array.copy(a(previous), 0, a(current), 0, w)
      for (j <- w to size)
        a(current)(j) = math.max(a(previous)(j), a(previous)(j - w) + v)
    }
    math.max(a(0)(size), a(1)(size))
  }

  /*
   * In this programming problem and the next you'll code up the greedy algorithm from the lectures on Huffman coding.
   * This file describes an instance of the problem. It has the following format:
   * [number_of_symbols]
   * [weight of symbol #1]
   * [weight of symbol #2]
   * ...
   * For example, the third line of the file is "6852892," indicating that the weight of the second symbol of the
   * alphabet is 6852892. (We're using weights instead of frequencies, like in the "A More Complex Example" video.)
   *
   * Your task in this problem is to run the Huffman coding algorithm from lecture on this data set.
   * What are the maximum and minimum lengths of a codeword in the resulting Huffman code?
   */
  def huffman(weights: Seq[Long]): (Int, Int) = {
    @tailrec
    def loop(pq: mutable.PriorityQueue[Node]): Node = {
      val x = pq.dequeue()
      if (pq.isEmpty) x
      else {
        val y = pq.dequeue()
        val max = math.max(x.max, y.max) + 1
        val min = math.min(x.min, y.min) + 1
        pq += Node(x.weight + y.weight, max, min)
        loop(pq)
      }
    }

    case class Node(weight: Long, max: Int, min: Int)

    val pq = collection.mutable.PriorityQueue
      .newBuilder[Node](Ordering.by[Node, Long](_.weight).reverse)

    weights
      .foreach(w => pq += Node(w, 0, 0))
    val root = loop(pq.result())

    (root.max, root.min)
  }

  /*
   * In this programming problem you'll code up the dynamic programming algorithm for computing a maximum-weight
   * independent set of a path graph.
   *
   * This file describes the weights of the vertices in a path graph (with the weights listed in the order
   * in which vertices appear in the path). It has the following format:
   *
   * [number_of_vertices]
   * [weight of first vertex]
   * [weight of second vertex]
   * ...
   * For example, the third line of the file is "6395702," indicating that the weight of the second vertex of the
   * graph is 6395702.
   *
   * Your task in this problem is to run the dynamic programming algorithm (and the reconstruction procedure)
   * from lecture on this data set.
   *
   * The question is: of the vertices 1, 2, 3, 4, 17, 117, 517, and 997, which ones belong to the maximum-weight
   * independent set? (By "vertex 1" we mean the first vertex of the graph---there is no vertex 0.)
   *
   * In the box below, enter a 8-bit string, where the ith bit should be 1 if the ith of these 8 vertices is
   * in the maximum-weight independent set, and 0 otherwise.
   * For example, if you think that the vertices 1, 4, 17, and 517 are in the maximum-weight independent set
   * and the other four vertices are not, then you should enter the string 10011010 in the box below.
   *
   * ANSWER: A BitSet in Scala is implemented as an Array[Long], where each bit signals the presence of a number
   * in the array. Long is 64 bit in Scala (on the JVM). One such Long can store values 0 to 63, the next one
   * after it 64 to 127, on so on. This is possible since we're only talking about unsigned numbers.
   * Given an example: BitSet(0, 2, 3), we can store all these numbers inside a single Long,
   * which in binary would be: 1101. Since we're in the range of 0 to 63, this works on a single Long value.
   *
   * In general, the upper limit, or the biggest value that can be stored in a BitSet in Scala is Int.MaxValue,
   * meaning 2^31-1 (2147483647). In order to store it, you'd need 2147483647 / 64 "bits" representing the number,
   * which is ~= 33554432 longs. This is why storing large numbers in a bit set can get quite expensive.
   */
  def mwis(weights: IndexedSeq[Long]): BitSet = {
    val a = Array.ofDim[Long](weights.size + 1)
    a(1) = weights.head

    for (i <- 2 to weights.size) {
      a(i) = math.max(a(i - 1), a(i - 2) + weights(i - 1))
    }

    Iterator.iterate((weights.size, -1)) { case (i, _) =>
      if (i <= 0) (-1, -1)
      else if (a(i) == a(i - 1)) (i - 1, -1)
      else if (i == 1) (i - 1, i)
      else (i - 2, i)
    }
      .drop(1)
      .takeWhile(_._1 >= 0)
      .filter(_._2 >= 0)
      .map(_._2)
      .foldLeft(BitSet.newBuilder)((acc, i) => acc += i)
      .result()
  }
}
