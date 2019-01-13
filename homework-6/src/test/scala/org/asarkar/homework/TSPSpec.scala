package org.asarkar.homework

import java.awt.geom.Point2D
import java.nio.file.Paths

import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source
import scala.util.{Failure, Success}

class TSPSpec extends FlatSpec with TableDrivenPropertyChecks {
  "nearestNeighbor" should "compute the min cost tour from the given files" in {
    val data =
    // format: off
      Table(("archive", "file", "sorted", "cost"),
        ("tsp-test", "tsp-test-1", false, 15.23d),
        ("tsp-test", "tsp-test-2", true, 191.31d),
        ("nn", "nn", true, 1203406.50d)
      )
    // format: on

    forAll(data) { (archive, file, sorted, cost) =>
      val path = Paths.get(getClass.getResource(s"/$archive.zip").toURI)

      ZipUtil.transformEntry(
        path,
        _ == s"$file.txt",
        is => {
          val it = Source.fromInputStream(is)
            .getLines
            .filterNot(_.isEmpty)

          val n = it.next().trim.toInt

          val points = it
            .map(_
              .split("\\s+")
              .map(_.trim) match {
              case Array(_, v, w) => new Point2D.Double(v.toDouble, w.toDouble)
              case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
            })
            .toIndexedSeq

          is.close()

          points
            .ensuring(_.size == n, s"Expected $n points, but got ${points.size}")
        }
      ) match {
        case Failure(e) => fail("Failed to read input file", e)
        case Success(points) =>
          TSP.nearestNeighbor(points, sorted) shouldBe (cost +- 0.01d)
      }
    }
  }
}
