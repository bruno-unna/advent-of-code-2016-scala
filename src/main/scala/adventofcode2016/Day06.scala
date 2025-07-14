package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.Try

object Day06 extends ZIOAppDefault:

  def errorCorrect(signal: Seq[String]): String =
    signal
      .map(_.toCharArray())
      .transpose
      .map: letterCandidates =>
        val letterGroups = letterCandidates.groupBy(identity)
        val letterCounts = letterGroups.keys
          .map: g =>
            (g, letterGroups(g).length)
          .toList
        letterCounts.maxBy(_._2)._1
      .mkString

  def run =
    for
      originalSignal <- Util.readStrings("/06.txt")
      correctedMessage = errorCorrect(originalSignal)

      _ = printf(
        "Day 06\n\terror-corrected message: %s\n\n",
        correctedMessage
      )
    yield ExitCode.success
