package adventofcode2016.day02

import munit.FunSuite

class Day02Suite extends FunSuite:
  test("calculate code"):
    val startingDigit = '5'
    val instructions = List(
      "ULL",
      "RRDDD",
      "LURDL",
      "UUUUD"
    )
    val code = calculateCode(instructions, startingDigit)
    assertEquals(code, "1985")
