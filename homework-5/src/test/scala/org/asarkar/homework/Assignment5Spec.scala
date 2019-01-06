package org.asarkar.homework

import java.awt.geom.Point2D
import java.nio.file.Paths

import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source
import scala.util.{Failure, Success}

class Assignment5Spec extends FlatSpec with TableDrivenPropertyChecks {
  "tsp" should "compute the min cost tour from the given graphs" in {
    val c1 = Map(
      1 -> Map(2 -> 4d, 3 -> 1d, 4 -> 3d),
      2 -> Map(3 -> 2d, 4 -> 1d),
      3 -> Map(4 -> 5d)
    )

    Assignment5.tsp(c1, 1) shouldBe (7.0 +- 0.01d)

    val c2 = Map(
      0 -> Map(2 -> 6d, 3 -> 1d, 4 -> 3d),
      2 -> Map(3 -> 4d, 4 -> 3d),
      3 -> Map(4 -> 2d)
    )

    Assignment5.tsp(c2, 0) shouldBe (11.0 +- 0.01d)
  }

  it should "compute the min cost tour from the assignment file" in {
    val path = Paths.get(getClass.getResource("/tsp.zip").toURI)

    ZipUtil.transformEntry(
      path,
      _ == s"tsp.txt",
      is => {
        val it = Source.fromInputStream(is)
          .getLines
          .filterNot(_.isEmpty)

        val n = it.next().trim.toInt

        val points = it
          .map(_
            .split("\\s+")
            .map(_.trim) match {
            case Array(u, w) => new Point2D.Double(u.toDouble, w.toDouble)
            case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
          })
          .toIndexedSeq

        val costs = points
          .indices
          .map(i => i -> (i + 1 until points.size).map(j => j -> points(i).distance(points(j))).toMap)
          .toMap

        is.close()

        costs
          .ensuring(_.size == n, s"Expected $n cities, but found: ${costs.size}")
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(costs) =>
        val left = costs.filter(_._1 <= 12)
        val right = costs.filter(_._1 >= 11)
        Assignment5.tsp(left, 0) + Assignment5.tsp(right, 11) - 2 * costs(11)(12) shouldBe (26442d +- 0.01d)
    }
  }
}
