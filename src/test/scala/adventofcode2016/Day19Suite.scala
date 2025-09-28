package adventofcode2016

import munit.FunSuite

class Day19Suite extends FunSuite:

  test("5 elves"):
    val actualWinner = Day19.reduce(5)
    val expectedWinner = 3
    assertEquals(actualWinner, expectedWinner)
