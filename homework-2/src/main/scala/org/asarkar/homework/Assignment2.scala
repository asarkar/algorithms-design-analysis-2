package org.asarkar.homework

import org.asarkar.data.UndirectedWeightedEdge
import org.asarkar.data.mutable.UnionFind

object Assignment2 {
  /*
   * 1. In this programming problem and the next you'll code up the clustering algorithm from lecture for computing
   * a max-spacing kk-clustering. Download the text file below. This file describes a distance function
   * (equivalently, a complete graph with edge costs). It has the following format:
   *
   * [number_of_nodes]
   * [edge 1 node 1] [edge 1 node 2] [edge 1 cost]
   * [edge 2 node 1] [edge 2 node 2] [edge 2 cost]
   * ...
   *
   * There is one edge (i,j) for each choice of 1 ≤ i ≤ j ≤ n, where n is the number of nodes.
   * For example, the third line of the file is "1 3 5250", indicating that the distance between nodes 1 and 3
   * (equivalently, the cost of the edge (1,3)) is 5250. You can assume that distances are positive,
   * but you should NOT assume that they are distinct.
   *
   * Your task in this problem is to run the clustering algorithm from lecture on this data set,
   * where the target number k of clusters is set to 4. What is the maximum spacing of a 4-clustering?
   */
  def clustering1(n: Int, k: Int, edges: Seq[UndirectedWeightedEdge[Int]]): Double = {
    val uf = UnionFind(n)

    edges
      .sortBy(_.weight)
      .iterator
      .map { e =>
        uf.union(e.tail, e.head)
        e.weight
      }
      .dropWhile(_ => uf.size >= k)
      .next()
  }

  /*
   * 2. In this question your task is again to run the clustering algorithm from lecture, but on a MUCH bigger graph.
   * So big, in fact, that the distances (i.e., edge costs) are only defined implicitly, rather than being provided
   * as an explicit list. The format is:
   * [# of nodes] [# of bits for each node's label]
   * [first bit of node 1] ... [last bit of node 1]
   * [first bit of node 2] ... [last bit of node 2]
   * ...
   * [number_of_nodes]
   * [edge 1 node 1] [edge 1 node 2] [edge 1 cost]
   * [edge 2 node 1] [edge 2 node 2] [edge 2 cost]
   * ...
   *
   * For example, the third line of the file "0 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 1 0 1 0 1 1 0 1" denotes the
   * 24 bits associated with node #2.
   *
   * The distance between two nodes u and v in this problem is defined as the Hamming distance ---
   * the number of differing bits --- between the two nodes' labels. For example, the Hamming distance between the
   * 24-bit label of node #2 above and the label "0 1 0 0 0 1 0 0 0 1 0 1 1 1 1 1 1 0 1 0 0 1 0 1" is 3
   * (since they differ in the 3rd, 7th, and 21st bits).
   *
   * The question is: what is the largest value of k such that there is a k-clustering with spacing at least 3?
   * That is, how many clusters are needed to ensure that no pair of nodes with all but 2 bits in common get
   * split into different clusters?
   *
   * NOTE: The graph implicitly defined by the data file is so big that you probably can't write it out explicitly,
   * let alone sort the edges by cost. So you will have to be a little creative to complete this part of the question.
   * For example, is there some way you can identify the smallest distances without explicitly looking at every
   * pair of nodes?
   *
   * ANSWER: Because of the size of the input sequence, we don't attempt to sort it, because we're going to need
   * about (200k choose 2 ≅ 20 billion) comparisons! Also, the question isn't about finding a max distance given
   * the number of clusters, but merging all vertices within two hops. That's a strong hint that calculating
   * distances greater than two hops isn't unnecessary.
   * Since the input numbers are given in binary format, and the distances as Hamming distance, we precalculate
   * the bitmasks for all n + (n choose 2) possible neighbors. For n = 24, that's 300; the neighbors are then given
   * by a simple XOR operation with each bitmask. I say "Possible neighbors" because not all of the computed ones
   * may be present in the input sequence, and we use a hash table to weed out the phantom ones.
   *
   * When done, we simply return the size of the Union-Find data structure, which is precisely the number of clusters.
   */
  def clustering2(n: Int, xs: Seq[String]): Int = {
    val range = 0 to n
    val masks = range
      .map(1 << _) ++
      range
        .combinations(2)
        .map(_.reduce((i, j) => (1 << i) | (1 << j)))
    val indices = collection.mutable.Map.empty[Int, Int]
    var count = 1

    xs
      .map { s =>
        require(s.length == n, s"$s must be of length $n")
        Integer.parseInt(s, 2)
      }
      .withFilter(i => !indices.contains(i))
      .foreach { i =>
        indices += (i -> count)
        count += 1
      }

    val uf = UnionFind(count - 1)

    indices
      .keys
      .foreach { i =>
        masks
          .map(i ^ _)
          .withFilter(indices.contains)
          .foreach(j => uf.union(indices(i), indices(j)))
      }

    uf.size
  }
}
