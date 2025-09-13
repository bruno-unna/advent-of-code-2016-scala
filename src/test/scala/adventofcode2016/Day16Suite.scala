package adventofcode2016

import munit.FunSuite

class Day16Suite extends FunSuite:

  test("Fill disc"):
    val discLength = 20
    val initState = "10000"
    val expectedFullDisc = "10000011110010000111"
    val actualFullDisc = Day16.fillDisc(20, initState)
    assertEquals(actualFullDisc, expectedFullDisc)

  test("Calculate checksum"):
    val expectedChecksum = "01100"
    val actualChecksum = Day16.calculateChecksum("10000011110010000111")
    assertEquals(expectedChecksum, actualChecksum)
