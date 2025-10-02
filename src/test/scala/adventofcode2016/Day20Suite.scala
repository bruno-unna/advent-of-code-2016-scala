package adventofcode2016

import munit.FunSuite

import scala.collection.immutable.NumericRange

class Day20Suite extends FunSuite:

  test("find first non-blocked address"):
    val blockedRanges: Vector[NumericRange[Long]] = Vector(5 to 8, 0 to 2, 4 to 7).map(r => r.start.toLong to r.end.toLong)
    val ipAddress = Day20.findFirstAllowedAddress(blockedRanges)
    assertEquals(ipAddress, 3L)
