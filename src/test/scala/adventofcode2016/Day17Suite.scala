package adventofcode2016

import adventofcode2016.Day17.{Coordinates, findAllPaths}
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

  test("longest path for ihgpwlah"):
    val expected = 370
    val actual = Day17.findAllPaths("ihgpwlah").map(_.length).max
    assertEquals(actual, expected)

  test("longest path for kglvqrro"):
    val expected = 492
    val actual = Day17.findAllPaths("kglvqrro").map(_.length).max
    assertEquals(actual, expected)

  test("longest path for ulqzkmiv"):
    val expected = 830
    val actual = Day17.findAllPaths("ulqzkmiv").map(_.length).max
    assertEquals(actual, expected)