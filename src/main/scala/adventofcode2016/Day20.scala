package adventofcode2016

import zio.*

import java.io.IOException
import scala.collection.immutable.NumericRange

/**
 * Solves Advent of Code Day 20: Firewall Rules.
 *
 * This problem involves finding the first unblocked IP address and counting the total
 * number of unblocked IP addresses given a list of blocked ranges.
 */
object Day20 extends ZIOAppDefault:

  /**
   * Solves Part 1: Finds the lowest-valued IP address that is not blocked.
   *
   * This is achieved by sorting the ranges and using `foldLeft` to track the
   * current maximum blocked address (`current`). The first time the current address
   * is less than a range's start, a gap is found and `current` is returned.
   *
   * @param ranges A vector of blocked IP address ranges.
   * @return The first valid (unblocked) IP address as a [[Long]].
   */
  def findFirstAllowedAddress(ranges: Vector[NumericRange[Long]]): Long =
    val sortedRanges = ranges.sorted
    sortedRanges.foldLeft(0L): (current, range) =>
      if current < range.start then current
      else if current < range.end then range.end + 1
      else current

  /**
   * Solves Part 2: Counts the total number of unblocked IP addresses.
   *
   * This is done by first merging all overlapping and contained ranges into a
   * minimal set of non-overlapping ranges, and then subtracting the total
   * blocked count from the total address space (2 to the power of 32).
   *
   * @param ranges A vector of blocked IP address ranges.
   * @return The total count of valid (unblocked) IP addresses as a [[Long]].
   */
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

  /**
   * The main program entry point for Day 20.
   *
   * Reads firewall rules from a file, solves both parts of the problem, and prints the results.
   */
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

  /**
   * Defines the ZIO application run method.
   */
  override def run: ZIO[Any, IOException | Option[Nothing], Unit] = program