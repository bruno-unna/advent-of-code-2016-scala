package adventofcode2016.day02

import zio._
import adventofcode2016.Util

extension (digit: Char)
  def affect(direction: Char): Char =
    direction match
      case 'U' =>
        digit match
          case top @ ('1' | '2' | '3') => top
          case rest @ _                => (rest - 3).toChar
      case 'D' =>
        digit match
          case bottom @ ('7' | '8' | '9') => bottom
          case rest @ _                   => (rest + 3).toChar
      case 'L' =>
        digit match
          case left @ ('1' | '4' | '7') => left
          case rest @ _                 => (rest - 1).toChar
      case 'R' =>
        digit match
          case right @ ('3' | '6' | '9') => right
          case rest @ _                  => (rest + 1).toChar

extension (s: String)
  def toDigit(fromDigit: Char): Char =
    if s.isEmpty() then fromDigit
    else
      val newDigit = fromDigit.affect(s.head)
      s.substring(1).toDigit(newDigit)

def calculateCode(instructions: Seq[String], startingDigit: Char): String =
  val (_, code) =
    instructions.foldLeft[(Char, String)]((startingDigit, ""))((t, s) => {
      val (prevDigit, acc) = t
      val newDigit = s.toDigit(prevDigit)
      (newDigit, acc + newDigit)
    })
  code

object Day02 extends ZIOAppDefault:
  def run =
    for
      instructions <- Util.readStrings("/02.txt")
      naiveCode = calculateCode(instructions, '5')
      _ = printf(
        "Day 02\n\tnaive code for bathroom: %d\n\treal code: %s\n",
        naiveCode,
        "0000"
      )
    yield naiveCode
