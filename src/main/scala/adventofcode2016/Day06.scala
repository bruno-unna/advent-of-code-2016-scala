package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.Try

object Day06 extends ZIOAppDefault:

  def errorCorrect(
      signal: Seq[String],
      f: Seq[(Char, Int)] => Char
  ): String =
    signal
      .map(_.toCharArray())
      .transpose
      .map: letterCandidates =>
        val letterGroups = letterCandidates.groupBy(identity)
        val letterCounts = letterGroups.keys
          .map: g =>
            (g, letterGroups(g).length)
          .toList
        f(letterCounts)
      .mkString

  val maximum: Seq[(Char, Int)] => Char = tuples => tuples.maxBy(_._2)._1

  val minimum: Seq[(Char, Int)] => Char = tuples => tuples.minBy(_._2)._1

  def run =
    for
      originalSignal <- Util.readStrings("/06.txt")
      correctedMessageByMax = errorCorrect(originalSignal, maximum)
      correctedMessageByMin = errorCorrect(originalSignal, minimum)

      _ = printf(
        "Day 06\n\tmost-likely error-corrected message: %s\n\tleast-likely error-corrected message: %s\n",
        correctedMessageByMax,
        correctedMessageByMin
      )
    yield ExitCode.success
