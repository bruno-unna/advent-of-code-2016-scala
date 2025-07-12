package adventofcode2016

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
    val code = calculateCode(instructions, startingDigit, standardKeypad)
    assertEquals(code, "1985")

    val realCode = calculateCode(instructions, startingDigit, funnyKeypad)
    assertEquals(realCode, "5DB3")
