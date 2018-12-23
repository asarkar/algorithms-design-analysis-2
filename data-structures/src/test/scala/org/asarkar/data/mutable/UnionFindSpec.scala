package org.asarkar.data.mutable

import java.nio.file.Paths

import org.asarkar.test.ZipUtil.transformEntry
import org.scalatest.Matchers._
import org.scalatest.{AppendedClues, FlatSpec}

import scala.io.Source
import scala.util.{Failure, Success}

class UnionFindSpec extends FlatSpec with AppendedClues {
  "UnionFind" should "find connected components" in {
    val path = Paths.get(getClass.getResource("/uf.zip").toURI)
    transformEntry(
      path,
      _ == "uf.txt",
      is => {
        val uf = UnionFind(10)
        Source.fromInputStream(is)
          .getLines
          .filterNot(_.isEmpty)
          .map(line => line
            .split("\\s+")
            .map(_.trim.toInt) match {
            case Array(p, q) => uf.union(p, q)
            case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
          })
          .toList

        is.close()
        uf
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(uf) =>
        uf.size shouldBe 2
        Seq(Seq(4, 5, 9, 10), Seq(1, 2, 3, 6, 7, 8))
          .foreach(_.combinations(2).foreach {
            case Seq(p, q) => {
              uf.isConnected(p, q) shouldBe true
            } withClue s", $p and $q are not connected"
            case _ => fail("Shouldn't be here")
          })
    }
  }
}
