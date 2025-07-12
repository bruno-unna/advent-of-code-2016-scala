package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.Try
import scala.util.matching.Regex

case class Room(encryptedName: String, sector: Int, checksum: String):
  def isValid: Boolean =
    val chars = encryptedName.replaceAll("-", "").toCharArray().sorted
    val counts = chars
      .groupBy(identity)
      .map:
        case (char, listOfChars) => (char, listOfChars.length)
    val sortedKeys = counts.keys.toList.sortWith: (k1, k2) =>
      if counts(k1) == counts(k2) then k1 < k2
      else counts(k1) > counts(k2)
    val calculatedChecksum = sortedKeys.take(5).mkString
    checksum == calculatedChecksum

  def decrypt: String =
    val offset = sector % 26
    encryptedName
      .toCharArray()
      .map:
        case '-'   => ' '
        case c @ _ => (((c - 'a') + offset) % 26 + 'a').toChar
      .mkString

object Room:
  val RoomRE: Regex = """((?:[a-z]+-)+)(\d+)\[([a-z]+)\]""".r

  def fromString(s: String): Option[Room] =
    s match
      case RoomRE(name, sector, checksum) =>
        Some(Room(name.dropRight(1), sector.toInt, checksum))
      case _ => None

object Day04 extends ZIOAppDefault:

  def sumValidSectors(rooms: Seq[Room]): Int =
    rooms.filter(_.isValid).map(_.sector).sum

  def run =
    for
      roomNames <- Util.readStrings("/04.txt")

      rooms =
        roomNames
          .map(Room.fromString(_))
          .collect { case Some(room) => room }
      sumOfSectors = sumValidSectors(rooms)

      northPoleRoom = rooms
        .find(_.decrypt.startsWith("north"))

      (northPoleRoomName, northPoleRoomSector) = northPoleRoom match
        case None    => ("?", 0)
        case Some(r) => (r.decrypt, r.sector)

      _ = printf(
        "Day 04\n\tsum of sector IDs: %d\n\tRoom \"%s\"'s sector: %d\n",
        sumOfSectors,
        northPoleRoomName,
        northPoleRoomSector
      )
    yield (sumOfSectors, northPoleRoom)
