package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.Try

object Day06 extends ZIOAppDefault:

  def errorCorrect(signal: Seq[String]): String = ???

  def run =
    for
      originalSignal <- Util.readStrings("/06.txt")
      correctedMessage = errorCorrect(originalSignal)

      _ = printf(
        "Day 06\n\terror-corrected message: %s\n\n",
        correctedMessage
      )
    yield ExitCode.success
