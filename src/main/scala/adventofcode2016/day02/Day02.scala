package adventofcode2016.day02

import zio._
import adventofcode2016.Util
import scala.util.matching.Regex

def calculateCode(instructions: Seq[String], startingDigit: Int): Int = ???

object Day02 extends ZIOAppDefault:
  def run =
    val startingDigit = 5
    for
      instructions <- Util.readStrings("/02.txt")
      code = calculateCode(instructions, startingDigit)
      _ = printf(
        "Day 02\n\tcode for bathroom: %d\n\tblah, blah: %d\n",
        code,
        0
      )
    yield code
