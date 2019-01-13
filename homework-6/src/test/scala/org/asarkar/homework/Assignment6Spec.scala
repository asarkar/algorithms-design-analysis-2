package org.asarkar.homework

import java.nio.file.Paths

import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source
import scala.util.{Failure, Success}

// more test cases here: https://github.com/beaunus/stanford-algs/tree/master/testCases/course4/assignment4TwoSat
class Assignment6Spec extends FlatSpec with TableDrivenPropertyChecks {
  "twoSat" should "determine satisfiability for the given instances" in {
    val xs1 = true -> Seq(
      1 -> 2,
      1 -> 2,
      2 -> 3
    )
    val xs2 = true -> Seq(
      1 -> -2,
      -1 -> -2,
      2 -> -3
    )

    val xs3 = true -> Seq(
      1 -> 2,
      -1 -> 3,
      3 -> 4,
      -2 -> -4
    )

    val xs4 = false -> Seq(
      1 -> -2,
      -1 -> 2,
      -2 -> 4,
      -2 -> -4,
      2 -> 4,
      2 -> -4
    )

    Seq(xs1, xs2, xs3, xs4)
      .forall(x => Assignment6.twoSat(x._2) == x._1)
  }

  it should "determine satisfiability from the assignment files" in {
    val path = Paths.get(getClass.getResource("/2sat.zip").toURI)

    (1 to 6)
      .par
      .map { id =>
        ZipUtil.transformEntry(
          path,
          _ == s"2sat$id.txt",
          is => {
            val it = Source.fromInputStream(is)
              .getLines
              .filterNot(_.isEmpty)

            val n = it.next().trim.toInt

            val clauses = it
              .map(_
                .split("\\s+")
                .map(_.trim.toInt) match {
                case Array(u, w) => u -> w
                case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
              })
              .toList

            is.close()

            clauses
              .ensuring(_.size == n, s"Expected $n cities, but found: ${clauses.size}")
          }
        ) match {
          case Failure(e) => fail("Failed to read input file", e)
          case Success(clauses) => id - 1 -> (if (Assignment6.twoSat(clauses)) 1 else 0)
        }
      }
      .foldLeft(new StringBuilder(6)) { case (s, (i, j)) => s.insert(i, j) }
      .toString shouldBe "101100"
  }
}
