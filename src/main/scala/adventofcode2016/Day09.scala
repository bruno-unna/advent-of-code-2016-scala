package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.annotation.tailrec
import scala.util.matching.Regex

/** Main application object for Advent of Code 2016, Day 9. It provides
  * functionality to decompress a document based on specific rules, calculating
  * both the simple decompressed length and the fully (composably) decompressed
  * length.
  */
object Day09 extends ZIOAppDefault:
  /** Regular expression to parse a compression marker and its surrounding
    * parts. It captures:
    *   1. The prefix before the first marker (non-greedy). 2. The data length
    *      specified in the marker. 3. The repetitions specified in the marker.
    *      4. The suffix after the data segment following the marker.
    */
  val MarkerDataRE: Regex = """^(.*?)\((\d+)x(\d+)\)(.*)$""".r

  /** Decompresses a given string according to the rules of Part 1. In this
    * part, nested markers are treated as literal text and not further
    * decompressed.
    *
    * @param compressed
    *   The compressed input string.
    * @return
    *   The fully decompressed string.
    */
  def decompress(compressed: String): String =

    @tailrec
    def decompressRec(prefixAcc: String, compressed: String): String =
      if compressed.isBlank then prefixAcc
      else
        compressed.match
          case MarkerDataRE(prefix, dataLengthStr, repetitionsStr, suffix) =>
            val dataLength = dataLengthStr.toInt
            val repetitions = repetitionsStr.toInt

            val expandedSegment = suffix.take(dataLength) * repetitions

            val newPrefix = prefixAcc + prefix + expandedSegment
            val newSuffix = suffix.drop(dataLength)

            decompressRec(newPrefix, newSuffix)
          case _ =>
            prefixAcc + compressed

    decompressRec("", compressed)

  /** Calculates the length of a decompressed string according to the rules of
    * Part 2. In this part, nested markers within decompressed segments are
    * themselves recursively decompressed. This function calculates the length
    * without materialising the potentially very large decompressed string.
    *
    * @param compressed
    *   The compressed input string or segment.
    * @return
    *   The total length of the fully decompressed content as a `Long`.
    */
  def composableDecompress(compressed: String): Long =
    @tailrec
    def loop(
        currentSegment: String,
        currentMultiplier: Int,
        acc: Long,
        workStack: List[(String, Int)]
    ): Long =
      if currentSegment.isBlank then
        if workStack.isEmpty then
          // we're done
          acc
        else
          // current segment is done, but there's more work in the stack
          val ((nextSegment, nextMultiplier), newStack) =
            (workStack.head, workStack.tail)
          loop(nextSegment, nextMultiplier, acc, newStack)
      else
        currentSegment.match
          case MarkerDataRE(prefix, dataLengthStr, repetitionsStr, suffix) =>
            val dataLength = dataLengthStr.toInt
            val repetitions = repetitionsStr.toInt

            // Add the length of the literal prefix, considering the current multiplier
            val newAcc = acc + prefix.length * currentMultiplier

            // Divide the remaining work
            val segment = suffix.take(dataLength)
            val rest = suffix.drop(dataLength)

            // Push the rest of the segment onto the stack to process it eventually.
            // It retains the 'currentMultiplier'.
            val newWorkStack = (rest, currentMultiplier) :: workStack

            // Continue the loop with 'segment'. Its effective multiplier
            // is the current multiplier multiplied by the marker's repetitions.
            loop(segment, repetitions * currentMultiplier, newAcc, newWorkStack)

          case _ =>
            // Add the length of the literal characters, considering the current multiplier
            val newAcc = acc + currentSegment.length * currentMultiplier

            // If there's pending work on the stack, pop it and continue the loop
            if workStack.nonEmpty then
              val ((nextSegment, nextMultiplier), newStack) =
                (workStack.head, workStack.tail)
              loop(nextSegment, nextMultiplier, newAcc, newStack)
            else // No more string and no more work on stack, we are done
              newAcc

    loop(
      currentSegment = compressed,
      currentMultiplier = 1,
      acc = 0L,
      workStack = List.empty
    )

  /** The main entry point of the ZIO application for Day 9. Reads the
    * compressed document, calculates both simple and composable decompressed
    * lengths, and prints the results to the console.
    *
    * @return
    *   An `ExitCode` indicating the success or failure of the application.
    */
  def run =
    for
      compressed <- Util.readBigString("/09.txt")

      decompressed = decompress(compressed)
      simpleLength = decompressed.length

      composableLength = composableDecompress(compressed)

      _ <- Console.printLine(
        s"part 1: length of decompressed document is ${simpleLength}"
      )
      _ <- Console.printLine(
        s"part 2: length of (composable) decompressed document is ${composableLength}"
      )
    yield ExitCode.success
