package adventofcode2016

import munit.FunSuite
import adventofcode2016.Day07
import adventofcode2016.Day07.supportsTLS

class Day07Suite extends FunSuite:

  test("example 1"):
    val ip = "abba[mnop]qrst"
    assert(supportsTLS(ip))

  test("example 2"):
    val ip = "abcd[bddb]xyyx"
    assert(supportsTLS(ip) == false)

  test("example 3"):
    val ip = "aaaa[qwer]tyui"
    assert(supportsTLS(ip) == false)

  test("example 4"):
    val ip = "ioxxoj[asdfgh]zxcvbn"
    assert(supportsTLS(ip))
