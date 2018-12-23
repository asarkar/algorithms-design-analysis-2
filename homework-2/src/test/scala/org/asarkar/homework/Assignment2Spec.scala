package org.asarkar.homework

import java.nio.file.Paths

import org.asarkar.data.UndirectedWeightedEdge
import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source
import scala.util.{Failure, Success}

class Assignment2Spec extends FlatSpec with TableDrivenPropertyChecks {
  "Clustering1" should "find the max spacing from the test files" in {
    val path = Paths.get(getClass.getResource("/clustering1-test.zip").toURI)

    // https://github.com/beaunus/stanford-algs/tree/master/testCases/course3/assignment2Clustering/question1
    val data =
    // format: off
      Table(("ids", "v"),
        (1 to 4, 8),
        (5 to 8, 16),
        (9 to 12, 32),
        (13 to 16, 64),
        (17 to 20, 128)
      )
    // format: on

    forAll(data) { (ids, v) =>
      ids
        .map { id =>
          ZipUtil.transformEntry(
            path,
            _ == s"input-$id-$v.txt",
            is => {
              var n = -1
              val edges = collection.mutable.Set.empty[UndirectedWeightedEdge[Int]]
              Source.fromInputStream(is)
                .getLines
                .filterNot(_.isEmpty)
                .foreach(line => line
                  .split("\\s+")
                  .map(_.trim.toInt) match {
                  case Array(x) => n = x
                  case Array(u, w, weight) => edges += UndirectedWeightedEdge(u, w, weight)
                  case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
                })

              is.close()
              (n, edges.toSeq)
            }
          ) match {
            case Failure(e) => fail("Failed to read input file", e)
            case Success((n, edges)) => ZipUtil.transformEntry(
              path,
              _ == s"output-$id-$v.txt",
              is => {
                Source.fromInputStream(is)
                  .mkString
                  .trim
                  .toInt
              }
            ) match {
              case Failure(e) => fail("Failed to read output file", e)
              case Success(dist) => Assignment2.clustering1(n, 4, edges).intValue() shouldBe dist
            }
          }
        }
    }
  }

  it should "find the max spacing from the assignment file" in {
    val path = Paths.get(getClass.getResource("/clustering1.zip").toURI)
    ZipUtil.transformEntry(
      path,
      _ == s"clustering1.txt",
      is => {
        var n = -1
        val edges = collection.mutable.Set.empty[UndirectedWeightedEdge[Int]]
        Source.fromInputStream(is)
          .getLines
          .filterNot(_.isEmpty)
          .foreach(line => line
            .split("\\s+")
            .map(_.trim.toInt) match {
            case Array(x) => n = x
            case Array(u, w, weight) => edges += UndirectedWeightedEdge(u, w, weight)
            case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
          })

        is.close()
        (n, edges.toSeq)
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success((n, edges)) => Assignment2.clustering1(n, 4, edges).intValue() shouldBe 106
    }
  }

  "Clustering2" should "find the max spacing from the test files" in {
    // https://github.com/beaunus/stanford-algs/tree/master/testCases/course3/assignment2Clustering/question1
    val data =
    // format: off
      Table(("id", "n", "size"),
        (1, 4, 14),
        (2, 4, 10),
        (3, 4, 8),
        (4, 4, 6),
        (5, 4, 4),
        (6, 8, 12),
        (7, 8, 10),
        (8, 8, 8),
        (9, 8, 6),
        (10, 16, 18),
        (71, 8192, 18)
      )
    // format: on

    val path = Paths.get(getClass.getResource("/clustering2-test.zip").toURI)
    forAll(data) { (id, n, size) =>
      ZipUtil.transformEntry(
        path,
        _ == s"input-$id-$n-$size.txt",
        is => {
          var i = -1
          val it = Source.fromInputStream(is)
            .getLines
            .filterNot(_.isEmpty)
          if (it.hasNext) {
            i = it.next()
              .split("\\s+")
              .tail
              .map(_.trim.toInt)
              .head
          }
          val xs = it
            .map(_.replaceAll("\\s+", ""))
            .toList
          is.close()

          (i, xs)
        }
      ) match {
        case Failure(e) => fail("Failed to read input file", e)
        case Success((i, xs)) => ZipUtil.transformEntry(
          path,
          _ == s"output-$id-$n-$size.txt",
          is => {
            val j = Source.fromInputStream(is)
              .mkString
              .trim
              .toInt
            is.close()

            j
          }
        ) match {
          case Failure(e) => fail("Failed to read output file", e)
          case Success(numClusters) => Assignment2.clustering2(i, xs).intValue() shouldBe numClusters
        }
      }
    }
  }

  it should "find the max spacing from the assignment file" in {
    val path = Paths.get(getClass.getResource("/clustering_big.zip").toURI)
    ZipUtil.transformEntry(
      path,
      _ == s"clustering_big.txt",
      is => {
        var i = -1
        val it = Source.fromInputStream(is)
          .getLines
          .filterNot(_.isEmpty)
        if (it.hasNext) {
          i = it.next()
            .split("\\s+")
            .tail
            .map(_.trim.toInt)
            .head
        }
        val xs = it
          .map(_.replaceAll("\\s+", ""))
          .toList
        is.close()

        (i, xs)
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success((i, xs)) => Assignment2.clustering2(i, xs).intValue() shouldBe 6118
    }
  }
}
