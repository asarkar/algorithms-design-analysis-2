package org.asarkar.test

import java.io.{IOException, InputStream}
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipFile}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object ZipUtil {
  def transformEntry[T](archive: Path, nameMatches: String => Boolean, transformer: InputStream => T): Try[T] =
    Try(new ZipFile(archive.toFile))
      .flatMap(
        file =>
          file.entries.asScala.find(e => nameMatches(name(e))) match {
            case Some(e) =>
              file.getInputStream(e) match {
                case is: Any => println(s"Found file: ${name(e)}"); Success(is)
                case _ => Failure(new IOException("Null content"))
              }
            case _ => Failure(new IllegalArgumentException("No file meets the given condition"))
          }
      )
      .map(is => {
        val x = transformer(is)
        if (is.available > 0) {
          System.err.println("Input stream hasn't been fully consumed!")
        } else {
          is.close()
        }

        x
      })

  private def name(entry: ZipEntry): String = {
    Some(entry.getName.lastIndexOf('/'))
      .filter(_ >= 0)
      .map(entry.getName.takeRight)
      .getOrElse(entry.getName)
  }
}
