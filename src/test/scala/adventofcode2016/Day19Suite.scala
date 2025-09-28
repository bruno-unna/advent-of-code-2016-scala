package adventofcode2016

import munit.FunSuite

class Day19Suite extends FunSuite:

  test("5 elves"):
    val actualWinner = Day19.play(nElves = 5)
    val expectedWinner = 3
    assertEquals(actualWinner, expectedWinner)

  test("5 elves - diagonally"):
    val actualWinner = Day19.playDiagonally(nElves = 5)
    val expectedWinner = 2
    assertEquals(actualWinner, expectedWinner)
