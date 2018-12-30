package org.asarkar

import java.io.IOException
import java.nio.file.Paths

import org.asarkar.test.ZipUtil
import org.jgrapht.Graph
import org.jgrapht.graph.{DefaultWeightedEdge, DirectedWeightedPseudograph}

import scala.io.Source
import scala.util.{Failure, Success}

package object homework {
  def fromFile(archive: String, file: String, firstVertex: Int): Graph[Int, DefaultWeightedEdge] = {
    val path = Paths.get(getClass.getResource(archive).toURI)

    ZipUtil.transformEntry(
      path,
      _ == file,
      is => {
        val it = Source.fromInputStream(is)
          .getLines
          .filterNot(_.isEmpty)

        val (n, m) = it.next()
          .split("\\s+")
          .map(_.trim.toInt) match {
          case Array(a, b) => (a, b)
          case bad => throw new IllegalArgumentException(s"""Unexpected first line: ${bad.deep.mkString(" ")}""")
        }

        var builder = DirectedWeightedPseudograph.createBuilder[Int, DefaultWeightedEdge](classOf[DefaultWeightedEdge])
        // in case some node is not connected
        builder = (firstVertex to (n - 1 + firstVertex))
          .foldLeft(builder) { (b, v) => b.addVertex(v) }
        builder = it.foldLeft(builder) { (b, line) =>
          line
            .split("\\s+")
            .map(_.trim) match {
            case Array(u, v, w) => b.addEdge(u.toInt, v.toInt, w.toDouble)
            case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
          }
        }
        val g = builder.buildAsUnmodifiable()

        is.close()
        val x = g.vertexSet.size
        val y = g.edgeSet.size
        g
          .ensuring(x == n && y == m, s"Expected $n vertices and $m edges, but found only $x and $y, respectively")
      }
    ) match {
      case Failure(e) => throw new IOException("Failed to read input file", e)
      case Success(g) => g
    }
  }
}
