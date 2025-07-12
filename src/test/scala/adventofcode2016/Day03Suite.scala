package adventofcode2016

import munit.FunSuite
import Day03._

class Day03Suite extends FunSuite:
  test("validate a good triangle"):
    val triangle = Triangle(3, 4, 5)
    assert(triangle.isValid, "3-4-5 was wrongly marked as invalid")

  test("invalidate a bad triangle"):
    val triangle = Triangle(5, 10, 25)
    assert(!triangle.isValid, "5-10-25 was wrongly marked as valid")
