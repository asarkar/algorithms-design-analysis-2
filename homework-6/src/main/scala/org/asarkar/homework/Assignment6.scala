package org.asarkar.homework

import org.asarkar.data.{DirectedEdge, Graph}

import scala.collection.mutable

/*
 * In this assignment you will implement one or more algorithms for the 2SAT problem.
 * Here are 6 different 2SAT instances:
 *
 * The file format is as follows. In each instance, the number of variables and the number of clauses is the same,
 * and this number is specified on the first line of the file. Each subsequent line specifies a clause via its
 * two literals, with a number denoting the variable and a "-" sign denoting logical "not".
 * For example, the second line of the first data file is "-16808 75250", which indicates the
 * clause (-16808 ⋀ 75250).
 * Your task is to determine which of the 6 instances are satisfiable, and which are unsatisfiable.
 * In the box below, enter a 6-bit string, where the ith bit should be 1 if the ith instance is satisfiable,
 * and 0 otherwise. For example, if you think that the first 3 instances are satisfiable and the last 3 are not,
 * then you should enter the string 111000 in the box below.
 *
 * DISCUSSION: This assignment is deliberately open-ended, and you can implement whichever 2SAT algorithm you want.
 * For example, 2SAT reduces to computing the strongly connected components of a suitable graph
 * (with two vertices per variable and two directed edges per clause, you should think through the details).
 * This might be an especially attractive option for those of you who coded up an SCC algorithm in Part 2 of
 * this specialization. Alternatively, you can use Papadimitriou's randomized local search algorithm.
 * (The algorithm from lecture is probably too slow as stated, so you might want to make one or more
 * simple modifications to it --- even if this means breaking the analysis given in lecture --- to ensure
 * that it runs in a reasonable amount of time.) A third approach is via backtracking. In lecture we mentioned
 * this approach only in passing; see Chapter 9 of the Dasgupta-Papadimitriou-Vazirani book,
 * for example, for more details.
 *
 * ANSWER: This implementation is based on computing the SCC in a directed graph. We use Tarjan's SCC algorithm.
 *
 * We perform a crucial optimization as a preprocessing step. Notice that if a literal is only present
 * as either positive or negative form (not both), it can be set to true or false, respectively, and
 * all the clauses it is included in are satisfied. Thus, such clauses can be removed without affecting
 * the overall satisfiability. This is implemented by creating two sets of literals, positive and negative,
 * taking their symmetric difference (https://en.wikipedia.org/wiki/Symmetric_difference),
 * and then excluding those clauses which have any of the two literals present in the symmetric difference.
 *
 * This process is performed recursively, until there is no further reduction in the number of clauses.
 *
 * We then run the SCC algorithm on the reduced set of clauses. If any literal and its negation are
 * both present in the same SCC, then the 2-SAT instance is not satisfiable. As a further performance
 * boosting step, the test solves the given assignment instances parallelly.
 *
 * See https://blogs.asarkar.com/algorithms-design-analysis/hw-4-opt/ for more details of the algorithm.
 */
object Assignment6 {
  def twoSat(clauses: Seq[(Int, Int)]): Boolean = {
    def reduce(xs: Seq[(Int, Int)]): Seq[(Int, Int)] = {
      val (pos, neg) = xs
        .foldLeft((mutable.Set.empty[Int], mutable.Set.empty[Int])) { case ((p, n), cl) =>
          cl
            .productIterator
            .map(_.asInstanceOf[Int])
            .foreach(x => (if (x >= 0) p else n) += math.abs(x))
          (p, n)
        }

      val symDiff = pos.diff(neg).union(neg.diff(pos))

      xs
        .filterNot(_.productIterator.exists(x => symDiff.contains(math.abs(x.asInstanceOf[Int]))))
    }

    val reduced = Iterator.iterate((clauses, false)) { case (xs, _) =>
      val ys = reduce(xs)
      (ys, ys.size == xs.size)
    }
      .dropWhile(!_._2)
      .take(1)
      .map(_._1)
      .next

    if (reduced.isEmpty) true
    else {
      val g = reduced.foldLeft(Graph.directed[Int, DirectedEdge[Int]]) { case (graph, (x, y)) =>
        graph
          .addEdge(DirectedEdge(-x, y))
          .addEdge(DirectedEdge(-y, x))
      }

      val scc = new TarjanSCC(g)

      !reduced
        .flatMap(x => Seq(x._1, x._2))
        .exists(x => scc.isConnected(x, -x))
    }
  }
}
