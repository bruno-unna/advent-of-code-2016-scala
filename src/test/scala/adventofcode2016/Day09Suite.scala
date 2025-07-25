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

  test("Decompression example 2"):
    val compressed = "A(1x5)BC"
    val decompressed = decompress(compressed)
    assertEquals(decompressed, "ABBBBBC")

  test("Decompression example 3"):
    val compressed = "(3x3)XYZ"
    val decompressed = decompress(compressed)
    assertEquals(decompressed, "XYZXYZXYZ")

  test("Decompression example 4"):
    val compressed = "A(2x2)BCD(2x2)EFG"
    val decompressed = decompress(compressed)
    assertEquals(decompressed, "ABCBCDEFEFG")

  test("Decompression example 5"):
    val compressed = "(6x1)(1x3)A"
    val decompressed = decompress(compressed)
    assertEquals(decompressed, "(1x3)A")

  test("Decompression example 6"):
    val compressed = "X(8x2)(3x3)ABCY"
    val decompressed = decompress(compressed)
    assertEquals(decompressed, "X(3x3)ABC(3x3)ABCY")
