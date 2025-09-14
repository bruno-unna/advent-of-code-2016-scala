package adventofcode2016

import scala.annotation.tailrec

object Day16 extends App:

  @tailrec
  def fillDisc(size: Int, a: String): String =
    if a.length >= size then a.substring(0, size)
    else fillDisc(size, a + "0" + a.reverse.replace('0', '_').replace('1', '0').replace('_', '1'))

  def calculateChecksum(str: String): String = ???

  @main def runDay16(): Unit =
    println("Day 16")

    val input = "10001110011110000"

    val checksum = calculateChecksum(input)
    println(s"checksum: $checksum")
