package adventofcode2016

object Day16 extends App:

  def fillDisc(size: Int, initState: String): String = ???

  def calculateChecksum(str: String): String = ???

  @main def runDay16(): Unit =
    println("Day 16")

    val input = "10001110011110000"

    val checksum = calculateChecksum(input)
    println(s"checksum: $checksum")
