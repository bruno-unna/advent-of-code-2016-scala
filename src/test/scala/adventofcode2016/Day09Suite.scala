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

  test("Composasble decompression example 1"):
    val compressed = "(3x3)XYZ"
    val decompressedLength = composableDecompress(compressed)
    assertEquals(decompressedLength, 9)

  test("Composasble decompression example 2"):
    val compressed = "X(8x2)(3x3)ABCY"
    val decompressedLength = composableDecompress(compressed)
    assertEquals(decompressedLength, 20)

  test("Composasble decompression example 3"):
    val compressed = "(27x12)(20x12)(13x14)(7x10)(1x12)A"
    val decompressedLength = composableDecompress(compressed)
    assertEquals(decompressedLength, 241920)

  test("Composasble decompression example 4"):
    val compressed = "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
    val decompressedLength = composableDecompress(compressed)
    assertEquals(decompressedLength, 445)
