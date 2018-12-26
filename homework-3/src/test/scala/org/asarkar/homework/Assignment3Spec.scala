package org.asarkar.homework

import java.nio.file.Paths

import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.BitSet
import scala.io.Source
import scala.util.{Failure, Success}

class Assignment3Spec extends FlatSpec with TableDrivenPropertyChecks {
  "knapsack" should "compute the optimal value for the given files" in {
    val data =
    // format: off
      Table(("filename", "size"),
        ("knapsack-test", 8),
        ("knapsack1", 2493893),
        ("knapsack_big", 4243395)
      )
    // format: on

    forAll(data) { (filename, size) =>
      val path = Paths.get(getClass.getResource(s"/$filename.zip").toURI)

      ZipUtil.transformEntry(
        path,
        _ == s"$filename.txt",
        is => {
          val items = IndexedSeq.newBuilder[(Int, Int)]
          val it = Source.fromInputStream(is)
            .getLines
            .filterNot(_.isEmpty)

          val w = it.next()
            .split("\\s+")
            .map(_.trim.toInt)
            .head
          it
            .foreach(line => line
              .split("\\s+")
              .map(_.trim.toInt) match {
              case Array(v, weight) => items += ((v, weight))
              case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
            })

          is.close()
          (w, items.result())
        }
      ) match {
        case Failure(e) => fail("Failed to read input file", e)
        case Success((w, items)) => Assignment3.knapsack(w, items) shouldBe size
      }
    }
  }

  "huffman" should "compute the max and min length of a codeword" in {
    val (max, min) = Assignment3.huffman(Seq(3, 2, 6, 8, 2, 6))
    max shouldBe 4
    min shouldBe 2
  }

  it should "compute the max and min length of a codeword from the assignment file" in {
    val path = Paths.get(getClass.getResource("/huffman.zip").toURI)

    ZipUtil.transformEntry(
      path,
      _ == "huffman.txt",
      is => {
        Source.fromInputStream(is)
          .getLines
          .filterNot(_.isEmpty)
          .drop(1)
          .map(_.trim.toLong)
          .toList
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(weights) => Assignment3.huffman(weights) shouldBe(19, 9)
    }
  }

  "mwis" should "compute a maximum-weight independent set from the test files" in {
    // format: off
    val data = Table(("archiveName", "filename", "vertices"),
      ("mwis-test", "mwis-test-1", BitSet(2, 4, 6, 8, 10)),
      ("mwis-test", "mwis-test-2", BitSet(1, 3, 6, 9))
    )
    // format: on

    forAll(data) { (archiveName, filename, vertices) =>
      val path = Paths.get(getClass.getResource(s"/$archiveName.zip").toURI)

      ZipUtil.transformEntry(
        path,
        _ == s"$filename.txt",
        is => {
          val it = Source.fromInputStream(is)
            .getLines
            .filterNot(_.isEmpty)
            .map(_.trim.toLong)

          val (n, weights) = (it.next(), it.toIndexedSeq)
          scala.Predef.assert(weights.size == n, s"Expected $n weights, got ${weights.size}")
          (n, weights)
        }
      ) match {
        case Failure(e) => fail("Failed to read input file", e)
        case Success((n, weights)) =>
          Assignment3.mwis(weights) shouldBe vertices
      }
    }
  }

  it should "compute a maximum-weight independent set from the assignment file" in {
    val path = Paths.get(getClass.getResource("/mwis.zip").toURI)
    ZipUtil.transformEntry(
      path,
      _ == "mwis.txt",
      is => {
        val it = Source.fromInputStream(is)
          .getLines
          .filterNot(_.isEmpty)
          .map(_.trim.toLong)

        val (n, weights) = (it.next(), it.toIndexedSeq)
        scala.Predef.assert(weights.size == n, s"Expected $n weights, got ${weights.size}")
        (n, weights)
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success((n, weights)) =>
        val xs = Assignment3.mwis(weights)
        Seq(1, 2, 3, 4, 17, 117, 517, 997)
          .map(i => if (xs.contains(i)) 1 else 0)
          .mkString shouldBe "10100110"
    }
  }
}
