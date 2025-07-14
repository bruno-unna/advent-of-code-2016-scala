package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.util.Try

/** Solves Day 06 of Advent of Code 2016.
  *
  * This object contains methods to perform error correction on a signal based
  * on character frequencies in each column.
  */
object Day06 extends ZIOAppDefault:

  /** Performs error correction on a sequence of signal strings.
    *
    * The function processes the input `signal` by transposing its characters
    * (grouping characters by their original column position). For each column,
    * it counts the frequency of each character and then applies a provided
    * function `f` to select the characteristic character for that column.
    *
    * @param signal
    *   The input sequence of strings representing the signal. Each string is a
    *   row in the signal.
    * @param f
    *   A function that takes a sequence of (character, count) tuples and
    *   returns the chosen character for that column based on specific criteria
    *   (e.g., most frequent, least frequent).
    * @return
    *   The error-corrected message as a single string.
    */
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

  /** A function that selects the character with the maximum frequency from a
    * sequence of (character, count) tuples.
    *
    * This is used for "most-likely" error correction (Advent of Code Day 6 Part
    * 1).
    *
    * @param tuples
    *   A sequence of (character, count) pairs.
    * @return
    *   The character with the highest count.
    */
  val maximum: Seq[(Char, Int)] => Char = tuples => tuples.maxBy(_._2)._1

  /** A function that selects the character with the minimum frequency from a
    * sequence of (character, count) tuples.
    *
    * This is used for "least-likely" error correction (Advent of Code Day 6
    * Part 2).
    *
    * @param tuples
    *   A sequence of (character, count) pairs.
    * @return
    *   The character with the lowest count.
    */
  val minimum: Seq[(Char, Int)] => Char = tuples => tuples.minBy(_._2)._1

  /** The main entry point for the ZIO application.
    *
    * Reads the signal from a file, calculates both the most-likely and
    * least-likely error-corrected messages using the `errorCorrect` function
    * with `maximum` and `minimum` strategies, and prints the results to the
    * console.
    *
    * @return
    *   An `ExitCode` indicating the success or failure of the application.
    */
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
