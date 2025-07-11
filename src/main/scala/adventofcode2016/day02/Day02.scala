package adventofcode2016.day02

import zio._
import adventofcode2016.Util

extension (digit: Int)
  def affect(direction: Char): Int =
    direction match
      case 'U' =>
        digit match
          case top @ (1 | 2 | 3) => top
          case rest @ _          => rest - 3
      case 'D' =>
        digit match
          case bottom @ (7 | 8 | 9) => bottom
          case rest @ _             => rest + 3
      case 'L' =>
        digit match
          case left @ (1 | 4 | 7) => left
          case rest @ _           => rest - 1
      case 'R' =>
        digit match
          case right @ (3 | 6 | 9) => right
          case rest @ _            => rest + 1

extension (s: String)
  def toDigit(fromDigit: Int): Int =
    if s.isEmpty() then fromDigit
    else
      val newDigit = fromDigit.affect(s.head)
      s.substring(1).toDigit(newDigit)

def calculateCode(instructions: Seq[String], startingDigit: Int): Int =
  val (_, code) =
    instructions.foldLeft[(Int, Int)]((startingDigit, 0))((t, s) => {
      val (prevDigit, acc) = t
      val newDigit = s.toDigit(prevDigit)
      (newDigit, acc * 10 + newDigit)
    })
  code

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
