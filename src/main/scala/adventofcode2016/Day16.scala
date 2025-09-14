package adventofcode2016

import scala.annotation.tailrec

object Day16 extends App:

  @tailrec
  def fillDisc(size: Int, a: String): String =
    if a.length >= size then a.substring(0, size)
    else fillDisc(size, a + "0" + a.reverse.map {
      case '0' => '1'
      case '1' => '0'
    })

  @tailrec
  def calculateChecksum(str: String): String =
    if str.length % 2 == 1 then str
    else calculateChecksum(str.sliding(2, 2).map(c => c.charAt(0) == c.charAt(1)).map(if _ then '1' else '0').mkString(""))

  @main def runDay16(): Unit =
    println("Day 16")

    val input = "10001110011110000"

    val firstDisc = fillDisc(272, input)
    val firstChecksum = calculateChecksum(firstDisc)
    println(s"first disc checksum: $firstChecksum")

    val secondDisc = fillDisc(35651584, input)
    val secondChecksum = calculateChecksum(secondDisc)
    println(s"second disc checksum: $secondChecksum")
