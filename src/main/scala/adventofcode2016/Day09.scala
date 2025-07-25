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

  def run =
    for
      compressed <- Util.readBigString("/09.txt")
      decompressed = decompress(compressed)
      length = decompressed.length
      _ <- Console.printLine(s"length of decompressed document is ${length}")
    yield ExitCode.success
