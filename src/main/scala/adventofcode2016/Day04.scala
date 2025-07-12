package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.Try
import scala.util.matching.Regex

/** Represents a Room from the Advent of Code Day 4 puzzle.
  *
  * @param encryptedName
  *   The encrypted name of the room, potentially containing hyphens.
  * @param sector
  *   The sector ID of the room.
  * @param checksum
  *   The five-letter checksum provided for the room.
  */
case class Room(encryptedName: String, sector: Int, checksum: String):
  /** Checks if this room is a "real" room by validating its checksum.
    *
    * The checksum is calculated by taking the five most common letters in the
    * encrypted name (excluding hyphens), then breaking ties by alphabetical
    * order.
    *
    * @return
    *   `true` if the calculated checksum matches the provided checksum, `false`
    *   otherwise.
    */
  def isValid: Boolean =
    val chars = encryptedName.replaceAll("-", "").toCharArray().sorted

    val counts = chars
      .groupBy(identity)
      .map:
        case (char, listOfChars) => (char, listOfChars.length)

    val sortedKeys = counts.toSeq
      .sortBy:
        case (char, count) => (-count, char)
      .map(_._1)

    val calculatedChecksum = sortedKeys.take(5).mkString

    checksum == calculatedChecksum

  /** Decrypts the room's encrypted name using a Caesar cipher.
    *
    * Each letter is shifted by its sector ID modulo 26. Hyphens are replaced
    * with spaces.
    *
    * @return
    *   The decrypted name of the room.
    */
  def decrypt: String =
    val offset = sector % 26
    encryptedName
      .toCharArray()
      .map:
        case '-'   => ' '
        case c @ _ => (((c - 'a') + offset) % 26 + 'a').toChar
      .mkString

/** Companion object for the [[Room]] case class, providing utility methods.
  */
object Room:
  /** Regular expression used to parse a room string into its components:
    * encrypted name, sector ID, and checksum.
    */
  val RoomRE: Regex = """((?:[a-z]+-)+)(\d+)\[([a-z]+)\]""".r

  /** Attempts to parse a string into a [[Room]] object.
    *
    * The input string is expected to be in the format
    * "encrypted-name-123[checksum]".
    *
    * @param s
    *   The string to parse.
    * @return
    *   An `Option` containing the parsed [[Room]] if the string matches the
    *   expected format, otherwise `None`.
    */
  def fromString(s: String): Option[Room] =
    s match
      case RoomRE(name, sector, checksum) =>
        Some(Room(name.dropRight(1), sector.toInt, checksum))
      case _ => None

/** Main application object for Advent of Code 2016 - Day 04.
  *
  * This object extends `ZIOAppDefault`, making it a runnable ZIO application.
  * It reads room names from "/04.txt" and solves two challenges:
  *   1. Calculates the sum of sector IDs of all real (valid) rooms. 2. Finds
  *      the sector ID of the room whose decrypted name refers to "northpole
  *      object storage".
  */
object Day04 extends ZIOAppDefault:

  /** Calculates the sum of sector IDs for a sequence of rooms that are valid.
    *
    * @param rooms
    *   A sequence of [[Room]] objects.
    * @return
    *   The total sum of sector IDs for all valid rooms.
    */
  def sumValidSectors(rooms: Seq[Room]): Int =
    rooms.filter(_.isValid).map(_.sector).sum

  /** The main ZIO effect that constitutes the application's logic for Day 04.
    *
    * It performs the following steps:
    *   1. Reads the room name strings from the resource "/04.txt" using
    *      `Util.readStrings`. 2. Parses these strings into [[Room]] objects,
    *      filtering out invalid formats. 3. Calculates the sum of sector IDs of
    *      all valid rooms. 4. Finds the specific room whose decrypted name
    *      starts with "north" (representing "northpole object storage"). 5.
    *      Prints both the sum of sector IDs and the decrypted name and sector
    *      of the North Pole room.
    *
    * @return
    *   A `ZIO` effect that, when executed, performs the day's calculation and
    *   prints results. It yields a tuple containing the sum of valid sector IDs
    *   and the `Option` of the North Pole room.
    */
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
