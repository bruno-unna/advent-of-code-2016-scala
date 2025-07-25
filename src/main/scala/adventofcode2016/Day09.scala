package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.matching.Regex

object Day09 extends ZIOAppDefault:
  def decompress(compressed: String): String = ???

  def run =
    for
      compressed <- Util.readBigString("/09.txt")
      decompressed = decompress(compressed)
      length = decompressed.length
      _ <- Console.printLine(s"length of decompressed document is ${length}")
    yield ExitCode.success
