package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.Try

case class Room(encryptedName: String, sector: Int, checksum: String):
  def isValid: Boolean = ???

object Room:
  def fromString(s: String): Option[Room] = ???

object Day04 extends ZIOAppDefault:

  def sumValidSectors(roomNamesList: Seq[String]): Int = ???

  def run =
    for
      roomNames <- Util.readStrings("/03.txt")

      sumOfSectors = sumValidSectors(roomNames)

      _ = printf(
        "Day 04\n\tsum of sector IDs: %d\n\t???: %d\n",
        sumOfSectors,
        0
      )
    yield (sumOfSectors)
