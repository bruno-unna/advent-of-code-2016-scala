package adventofcode2016

import munit.FunSuite
import adventofcode2016.Day06.{errorCorrect, maximum, minimum}

class Day06Suite extends FunSuite:
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

  test("error-correct a message using most likeliness"):
    val message = errorCorrect(signal, maximum)
    assertEquals(message, "easter")

  test("error-correct a message using least likeliness"):
    val message = errorCorrect(signal, minimum)
    assertEquals(message, "advent")
