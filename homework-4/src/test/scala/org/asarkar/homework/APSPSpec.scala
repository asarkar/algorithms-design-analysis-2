package org.asarkar.homework

import java.nio.file.Paths

import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source
import scala.util.{Failure, Success}

class APSPSpec extends FlatSpec with TableDrivenPropertyChecks {
  // https://github.com/beaunus/stanford-algs/tree/master/testCases/course4/assignment1AllPairsShortestPath
  private val data =
  // format: off
    Table(("ids", "v"),
      (2 to 4, 2),
      (5 to 8, 4),
      (9 to 12, 8),
      (13 to 16, 16),
      (17 to 20, 32)
    )
  // format: on
  private val path = Paths.get(getClass.getResource("/apsp-test.zip").toURI)

  "FloydWarshall" should "compute the shortest paths or indicate negative cycle" in {
    forAll(data) { (ids, v) =>
      ids.map { id =>
        ZipUtil.transformEntry(
          path,
          _ == s"output-$id-$v.txt",
          is => {
            val min = Source.fromInputStream(is)
              .mkString
              .trim
            if (min == "NULL") Double.NegativeInfinity else min.toInt
          }
        ) match {
          case Failure(e) => fail("Failed to read output file", e)
          case Success(min) =>
            val fw = APSP.floydWarshall(fromFile("/apsp-test.zip", s"input-$id-$v.txt", 1))
            fw.min shouldBe min
            fw.hasNegativeCycle shouldBe (min == Double.NegativeInfinity)
        }
      }
    }
  }

  "Johnson" should "compute the shortest paths or indicate negative cycle" in {
    forAll(data) { (ids, v) =>
      ids.map { id =>
        ZipUtil.transformEntry(
          path,
          _ == s"output-$id-$v.txt",
          is => {
            val min = Source.fromInputStream(is)
              .mkString
              .trim
            if (min == "NULL") Double.NegativeInfinity else min.toInt
          }
        ) match {
          case Failure(e) => fail("Failed to read output file", e)
          case Success(min) =>
            val fw = APSP.johnson(fromFile("/apsp-test.zip", s"input-$id-$v.txt", 1))
            fw.min shouldBe min
            fw.hasNegativeCycle shouldBe (min == Double.NegativeInfinity)
        }
      }
    }
  }
}
