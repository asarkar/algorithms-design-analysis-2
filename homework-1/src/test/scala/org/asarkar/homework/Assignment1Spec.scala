package org.asarkar.homework

import java.nio.file.Paths

import org.asarkar.data.{Graph, UndirectedWeightedEdge}
import org.asarkar.homework.Assignment1.Job
import org.asarkar.test.ZipUtil.transformEntry
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source
import scala.util.{Failure, Success}

class Assignment1Spec extends FlatSpec with TableDrivenPropertyChecks {
  "Assignment1" should "schedule jobs by given order" in {
    val byDifference = Ordering.by[Job, (Int, Int)](t => (t.weight - t.length, t.weight)).reverse
    val byRatio = Ordering.by[Job, Double](t => (t.weight * 1.0d) / t.length).reverse

    val data =
    // format: off
      Table(("file", "ordering", "sum"),
        ("jobs-test", byDifference, 68615L),
        ("jobs-test", byRatio, 67247L),
        ("jobs", byDifference, 69119377652L),
        ("jobs", byRatio, 67311454237L)
      )
    // format: on

    forAll(data) { (file, ordering, sum) =>
      val path = Paths.get(getClass.getResource(s"/$file.zip").toURI)
      transformEntry(
        path,
        _ == s"$file.txt",
        is => {
          val jobs = Source.fromInputStream(is)
            .getLines
            .filterNot(_.isEmpty)
            .drop(1)
            .map(line => line
              .split("\\s+")
              .map(_.trim.toInt) match {
              case Array(w, l) => Job(w, l)
              case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
            })
            .toList

          is.close()
          jobs
        }
      ) match {
        case Failure(e) => fail("Failed to read input file", e)
        case Success(xs) => Assignment1.scheduling(xs)(ordering) shouldBe sum
      }
    }
  }

  it should "compute a MST using Prim's algorithm" in {
    val data =
    // format: off
      Table(("file", "mstSize"),
        ("mst-test", 37),
        ("edges", -3612829)
      )
    // format: on

    forAll(data) { (file, mstSize) =>
      val path = Paths.get(getClass.getResource(s"/$file.zip").toURI)
      transformEntry(
        path,
        _ == s"$file.txt",
        is => {
          val g = Graph.undirectedBuilder[Int, UndirectedWeightedEdge[Int]]()
          Source.fromInputStream(is)
            .getLines
            .filterNot(_.isEmpty)
            .foreach(line => line
              .split("\\s+")
              .map(_.trim) match {
              case Array(n, m) => println(s"|V| = $n, |E| = $m")
              case Array(v, w, weight) => g.addEdge(UndirectedWeightedEdge(v.toInt, w.toInt, weight.toInt))
              case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
            })

          is.close()
          g.build()
        }
      ) match {
        case Failure(e) => fail("Failed to read input file", e)
        case Success(g) => Assignment1.primsMST(g).intValue() shouldBe mstSize
      }
    }
  }
}
