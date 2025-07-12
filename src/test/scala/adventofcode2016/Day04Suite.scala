package adventofcode2016

import munit.FunSuite

class Day04Suite extends FunSuite:
  val roomNamesList = List(
    "aaaaa-bbb-z-y-x-123[abxyz]",
    "a-b-c-d-e-f-g-h-987[abcde]",
    "not-a-real-room-404[oarel]",
    "totally-real-room-200[decoy]"
  )
  test("validate real rooms"):
    for i <- 0 to 2 do
      val maybeRoom = Room.fromString(roomNamesList(i))
      assert(maybeRoom.isDefined)
      assert(maybeRoom.get.isValid)

  test("invalidate unreal rooms"):
    val maybeRoom = Room.fromString(roomNamesList(3))
    assert(maybeRoom.isDefined)
    assert(!maybeRoom.get.isValid)

  test("sum sector IDs"):
    val rooms =
      roomNamesList
        .map(Room.fromString(_))
        .collect { case Some(room) => room }
    val sumOfSectors = Day04.sumValidSectors(rooms)
    assertEquals(sumOfSectors, 1514)

  test("test decryption"):
    val encryptedName = "qzmt-zixmtkozy-ivhz-343[abcde]"
    val room = Room.fromString(encryptedName)
    assert(room.isDefined)
    val decryptedName = room.get.decrypt
    val expectedName = "very encrypted name"
    assertEquals(decryptedName, expectedName)
