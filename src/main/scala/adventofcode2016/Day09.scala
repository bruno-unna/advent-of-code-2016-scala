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
    if compressed.isBlank then 0
    else
      compressed.match
        case MarkerDataRE(prefix, dataLengthStr, repetitionsStr, suffix) =>
          val dataLength = dataLengthStr.toInt
          val repetitions = repetitionsStr.toInt

          val segment = suffix.take(dataLength)
          val rest = suffix.drop(dataLength)

          prefix.length +
            repetitions * composableDecompress(segment) +
            composableDecompress(rest)
        case _ =>
          compressed.length

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
