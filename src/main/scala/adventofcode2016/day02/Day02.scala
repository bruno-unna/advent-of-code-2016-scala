package adventofcode2016.day02

import zio._
import adventofcode2016.Util

type Keypad = (Char, Char) => Char

val standardKeypad: Keypad =
  (digit, direction) =>
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

val funnyKeypad: Keypad =
  (digit, direction) =>
    direction match
      case 'U' =>
        digit match
          case delta0 @ ('1' | '2' | '4' | '5' | '9') => delta0
          case delta2 @ ('3' | 'D')                   => (delta2 - 2).toChar
          case delta4 @ ('6' | '7' | '8')             => (delta4 - 4).toChar
          case delta11 @ ('A' | 'B' | 'C')            => (delta11 - 11).toChar
      case 'D' =>
        digit match
          case delta0 @ ('D' | 'A' | 'C' | '5' | '9') => delta0
          case delta2 @ ('B' | '1')                   => (delta2 + 2).toChar
          case delta4 @ ('2' | '3' | '4')             => (delta4 + 4).toChar
          case delta11 @ ('6' | '7' | '8')            => (delta11 + 11).toChar
      case 'L' =>
        digit match
          case left @ ('1' | '2' | '5' | 'A' | 'D') => left
          case rest @ _                             => (rest - 1).toChar
      case 'R' =>
        digit match
          case right @ ('1' | '4' | '9' | 'C' | 'D') => right
          case rest @ _                              => (rest + 1).toChar

extension (s: String)
  def toDigit(fromDigit: Char)(keypad: Keypad): Char =
    if s.isEmpty() then fromDigit
    else
      val newDigit = keypad(fromDigit, s.head)
      s.substring(1).toDigit(newDigit)(keypad)

def calculateCode(
    instructions: Seq[String],
    startingDigit: Char,
    keypad: Keypad
): String =
  val (_, code) =
    instructions.foldLeft[(Char, String)]((startingDigit, ""))((t, s) => {
      val (prevDigit, acc) = t
      val newDigit = s.toDigit(prevDigit)(keypad)
      (newDigit, acc + newDigit)
    })
  code

object Day02 extends ZIOAppDefault:
  def run =
    for
      instructions <- Util.readStrings("/02.txt")
      naiveCode = calculateCode(instructions, '5', standardKeypad)
      realCode = calculateCode(instructions, '5', funnyKeypad)
      _ = printf(
        "Day 02\n\tnaive code for bathroom: %s\n\treal code: %s\n",
        naiveCode,
        realCode
      )
    yield realCode
