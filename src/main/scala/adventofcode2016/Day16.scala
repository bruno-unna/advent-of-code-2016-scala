package adventofcode2016

import scala.annotation.tailrec

object Day16 extends App:

  @tailrec
  def fillDisc(size: Int, a: String): String =
    if a.length >= size then a.substring(0, size)
    else fillDisc(size, a + "0" + a.reverse.replace('0', '_').replace('1', '0').replace('_', '1'))

  @tailrec
  def calculateChecksum(str: String): String =
    if str.length % 2 == 1 then str
    else calculateChecksum(str.sliding(2, 2).map(c => c.charAt(0) == c.charAt(1)).map(if _ then '1' else '0').mkString(""))

  @main def runDay16(): Unit =
    println("Day 16")

    val input = "10001110011110000"

    val fullDisc = fillDisc(272, input)
    val checksum = calculateChecksum(fullDisc)

    println(s"checksum: $checksum")
