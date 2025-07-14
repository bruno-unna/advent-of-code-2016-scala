package adventofcode2016

import munit.FunSuite

class Day06Suite extends FunSuite:
  test("error-correct a message"):
    val signal = List(
      "eedadn",
      "drvtee",
      "eandsr",
      "raavrd",
      "atevrs",
      "tsrnev",
      "sdttsa",
      "rasrtv",
      "nssdts",
      "ntnada",
      "svetve",
      "tesnvt",
      "vntsnd",
      "vrdear",
      "dvrsen",
      "enarar"
    )
    val message = Day06.errorCorrect(signal)
    assertEquals(message, "easter")
