package adventofcode2016

import adventofcode2016.Day17.Coordinates
import munit.FunSuite

class Day17Suite extends FunSuite:

  test("ihgpwlah"):
    val expectedPath = "DDRRRD"
    val actualPath = Day17.findSteps("ihgpwlah")
    assertEquals(actualPath, expectedPath)

  test("kglvqrro"):
    val expectedPath = "DDUDRLRRUDRD"
    val actualPath = Day17.findSteps("kglvqrro")
    assertEquals(actualPath, expectedPath)

  test("ulqzkmiv"):
    val expectedPath = "DRURDRUDDLLDLUURRDULRLDUUDDDRR"
    val actualPath = Day17.findSteps("ulqzkmiv")
    assertEquals(actualPath, expectedPath)
