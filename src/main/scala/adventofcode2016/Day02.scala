package adventofcode2016

import zio._
import adventofcode2016.Util

/** Type alias for a function that represents a keypad's logic. It takes the
  * current digit and a direction, and returns the new digit.
  */
type Keypad = (Char, Char) => Char

/** Implements the logic for the standard 3x3 numeric bathroom keypad.
  *
  * Moving off the keypad's boundaries in any direction results in staying on
  * the current digit.
  *
  * Layout: 1 2 3 
  *         4 5 6 
  *         7 8 9
  */
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

/** Implements the logic for the non-standard diamond-shaped alphanumeric
  * keypad.
  *
  * Moving off the keypad's boundaries in any direction results in staying on
  * the current digit.
  *
  * Layout: 1 
  *       2 3 4 
  *     5 6 7 8 9 
  *       A B C 
  *         D
  */
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

/** Provides an extension method for `String` to process a sequence of movements
  * on a keypad.
  */
extension (s: String)
  /** Calculates the final digit reached by applying a sequence of directional
    * moves to a starting digit on a given keypad.
    *
    * This method processes the input string character by character (each
    * representing a direction like 'U', 'D', 'L', 'R') and applies the keypad
    * logic iteratively. If the input string is empty, the starting digit is
    * returned.
    *
    * @param fromDigit
    *   The starting digit on the keypad (e.g., '5').
    * @param keypad
    *   The [[Keypad]] function defining the movement rules.
    * @return
    *   The final `Char` digit reached after applying all moves in the string.
    */
  def toDigit(fromDigit: Char)(keypad: Keypad): Char =
    if s.isEmpty() then fromDigit
    else
      val newDigit = keypad(fromDigit, s.head)
      s.substring(1).toDigit(newDigit)(keypad)

/** Calculates a bathroom code by processing a sequence of instruction strings.
  *
  * Each instruction string represents a series of moves to find a single digit.
  * The output code is a concatenation of the digits found for each instruction.
  * The previous instruction's final digit becomes the starting digit for the
  * next instruction.
  *
  * @param instructions
  *   The `Seq` of `String`s, where each string is a sequence of moves (e.g.,
  *   "UDLR").
  * @param startingDigit
  *   The initial digit for the first instruction (e.g., '5').
  * @param keypad
  *   The [[Keypad]] function to use for movements (e.g., `standardKeypad` or
  *   `funnyKeypad`).
  * @return
  *   A `String` representing the calculated bathroom code.
  */
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

/** Main application object for Advent of Code 2016 - Day 02.
  *
  * This object extends `ZIOAppDefault`, making it a runnable ZIO application.
  * It reads bathroom code instructions from "/02.txt", calculates two different
  * codes based on a standard 3x3 keypad and a "funny" alphanumeric keypad, and
  * prints the results to the console.
  */
object Day02 extends ZIOAppDefault:
  /** The main ZIO effect that constitutes the application's logic for Day 02.
    *
    * It performs the following steps:
    *   1. Reads the movement instructions from the resource "/02.txt" using
    *      `Util.readStrings`. 2. Calculates the code using the
    *      `standardKeypad`, starting from '5'. 3. Calculates the code using the
    *      `funnyKeypad`, also starting from '5'. 4. Prints both calculated
    *      codes to the console.
    *
    * @return
    *   A `ZIO` effect that, when executed, performs the day's calculation and
    *   prints results. It yields the real code on success.
    */
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
