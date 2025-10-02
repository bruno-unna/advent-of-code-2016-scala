package adventofcode2016

import zio.*

import java.io.IOException
import scala.collection.immutable.NumericRange

object Day20 extends ZIOAppDefault:

  def findFirstAllowedAddress(ranges: Vector[NumericRange[Long]]): Long =
    val sortedRanges = ranges.sorted
    sortedRanges.foldLeft(0L): (current, range) =>
      if current < range.start then current
      else if current < range.end then range.end + 1
      else current

  def countValidAddresses(ranges: Vector[NumericRange.Inclusive[Long]]): Long =
    val normalisedRanges =
      ranges
        .sortBy(r => (r.start, r.end))
        .foldLeft(Vector.empty[NumericRange.Inclusive[Long]]): (acc, range) =>
          if acc.isEmpty then Vector(range)
          else
            val a = acc.last
            val b = range

            if b.end <= a.end then acc
            else if b.start <= a.end then acc.take(acc.length - 1) :+ (a.start to b.end)
            else acc :+ b
    val invalidAddressesCount = normalisedRanges.foldLeft(0L)((acc, range) => acc + (range.end - range.start + 1L))
    (1L << 32) - invalidAddressesCount

  private def program =
    for
      rangesAsStrings <- Util.readStrings("/20.txt").map(_.toVector)
      ranges = rangesAsStrings.map: rangeStr =>
        rangeStr.split('-') match
          case Array(startStr, endStr) =>
            val (start, end) = (startStr.toLong, endStr.toLong)
            start to end
      validAddress = findFirstAllowedAddress(ranges)
      _ <- Console.printLine(s"first valid IP address: $validAddress")
      validAddressesCount = countValidAddresses(ranges)
      _ <- Console.printLine(s"number of valid IP addresses: $validAddressesCount")
    yield ()

  override def run: ZIO[Any, IOException | Option[Nothing], Unit] = program
  