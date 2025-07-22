package adventofcode2016

import adventofcode2016.Day07.*
import munit.FunSuite

class Day07Suite extends FunSuite:

  test("TLS example 1"):
    val ip = "abba[mnop]qrst"
    assert(supportsTLS(ip))

  test("TLS example 2"):
    val ip = "abcd[bddb]xyyx"
    assert(supportsTLS(ip) == false)

  test("TLS example 3"):
    val ip = "aaaa[qwer]tyui"
    assert(supportsTLS(ip) == false)

  test("TLS example 4"):
    val ip = "ioxxoj[asdfgh]zxcvbn"
    assert(supportsTLS(ip))

  test("SSL example 1"):
    val ip = "aba[bab]xyz"
    assert(supportsSSL(ip))

  test("SSL example 2"):
    val ip = "xyx[xyx]xyx"
    assert(supportsSSL(ip) == false)

  test("SSL example 3"):
    val ip = "aaa[kek]eke"
    assert(supportsSSL(ip))

  test("SSL example 4"):
    val ip = "zazbz[bzb]cdb"
    assert(supportsSSL(ip))
