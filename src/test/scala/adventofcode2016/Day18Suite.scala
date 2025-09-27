package adventofcode2016

import munit.FunSuite

class Day18Suite extends FunSuite:

  test("..^^. - three rows"):
    val map = Day18.findMap(firstRow = "..^^.", rows = 3)
    val actualSafeTiles = Day18.countSafeTiles(map)
    val expectedSafeTiles = 6
    assertEquals(actualSafeTiles, expectedSafeTiles)

  test(".^^.^.^^^^ - ten rows"):
    val map = Day18.findMap(firstRow = ".^^.^.^^^^", rows = 10)
    val actualSafeTiles = Day18.countSafeTiles(map)
    val expectedSafeTiles = 38
    assertEquals(actualSafeTiles, expectedSafeTiles)
