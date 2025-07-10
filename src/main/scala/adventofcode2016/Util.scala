package adventofcode2016

import zio._
import zio.nio.file.{Path, Files}
import java.io.IOException
import java.nio.charset.StandardCharsets
import scala.io.Source

object Util:
  def readBigString(fileName: String): ZIO[Any, IOException, String] =

    val zioPathEffect: IO[Throwable, Path] = ZIO.attempt {
      val url = getClass.getResource("/01.txt")
      Path.fromJava(java.nio.file.Paths.get(url.toURI))
    }

    val contentFromZioPathEffect: ZIO[Any, IOException, String] =
      for {
        zioPath <- zioPathEffect.refineToOrDie[IOException]
        content <- Files
          .readAllBytes(zioPath)
          .map(chunk => new String(chunk.toArray, StandardCharsets.UTF_8))
      } yield content

    contentFromZioPathEffect
