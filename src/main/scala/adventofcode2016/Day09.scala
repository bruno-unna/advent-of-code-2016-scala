package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day09 extends ZIOAppDefault:
  val MarkerDataRE: Regex = """^(.*?)\((\d+)x(\d+)\)(.*)$""".r

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
