package adventofcode2016

import munit.FunSuite

class Day05Suite extends FunSuite:
  test("calculate the password"):
    val doorID = "abc"
    val password = Day05.calculatePassword(doorID)
    assertEquals(password, "18f47a30")

  test("calculate the second password"):
    val doorID = "abc"
    val password = Day05.calculateSecondPassword(doorID)
    assertEquals(password, "05ace8e3")
