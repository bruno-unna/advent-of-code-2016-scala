package adventofcode2016

import scala.annotation.tailrec

/**
 * Solves Advent of Code 2016, Day 16.
 */
object Day16 extends App:

  /**
   * Fills a disc with a dragon curve sequence until it reaches the desired size.
   *
   * The function `a` is a string of '0's and '1's. The function `b` is `a` reversed and then
   * with '0's and '1's swapped. The new `a` is then `a + "0" + b`.
   *
   * @param size The target size of the disc.
   * @param a    The initial string of '0's and '1's.
   * @return A string representing the filled disc, truncated to the specified size.
   */
  @tailrec
  def fillDisc(size: Int, a: String): String =
    if a.length >= size then a.substring(0, size)
    else fillDisc(size, a + "0" + a.reverse.map {
      case '0' => '1'
      case '1' => '0'
    })

  /**
   * Calculates the checksum of a string of '0's and '1's.
   *
   * The checksum is calculated by repeatedly taking pairs of characters. If the characters
   * are the same, the pair is replaced with '1'; otherwise, it's replaced with '0'.
   * This process continues until the checksum string has an odd length.
   *
   * @param str The input string for which to calculate the checksum.
   * @return The final checksum string.
   */
  @tailrec
  def calculateChecksum(str: String): String =
    if str.length % 2 == 1 then str
    else calculateChecksum(str.sliding(2, 2)
      .map(c => c.charAt(0) == c.charAt(1))
      .map(if _ then '1' else '0').mkString(""))

  /**
   * Main entry point for the Day 16 solution.
   */
  @main def runDay16(): Unit =
    println("Day 16")

    val input = "10001110011110000"

    val firstDisc = fillDisc(272, input)
    val firstChecksum = calculateChecksum(firstDisc)
    println(s"first disc checksum: $firstChecksum")

    val secondDisc = fillDisc(35651584, input)
    val secondChecksum = calculateChecksum(secondDisc)
    println(s"second disc checksum: $secondChecksum")