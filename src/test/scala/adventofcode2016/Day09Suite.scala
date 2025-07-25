package adventofcode2016

import adventofcode2016.Day09.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

import munit.FunSuite

class Day09Suite extends FunSuite:

  test("Decompression example 1"):
    val compressed = "ADVENT"
    val decompressed = decompress(compressed)
    assertEquals(decompressed, "ADVENT")
