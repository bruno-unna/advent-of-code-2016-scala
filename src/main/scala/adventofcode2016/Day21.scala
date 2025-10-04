package adventofcode2016

import adventofcode2016.Day21.Scrambler.Operation
import zio.{Console, Task, ZIO, ZIOAppDefault, ZLayer}

import scala.util.matching.Regex

/**
 * Solves Advent of Code Day 21: Scrambled Letters and Hash.
 *
 * This problem involves processing a password string through a sequence of
 * operations and then reversing those operations.
 */
object Day21 extends ZIOAppDefault:

  /**
   * Defines the Scrambler module, containing the `Operation` ADT and the `Service` trait.
   */
  object Scrambler:
    /**
     * The sealed trait representing a single scrambling operation.
     * All concrete operations must extend this trait.
     */
    sealed trait Operation:
      /**
       * Applies the scrambling operation to the given input string.
       * @param input The password string to scramble.
       * @return The resulting scrambled string.
       */
      def apply(input: String): String

      /**
       * Reverses the operation, effectively "unscrambling" the string.
       * For reversible operations, this often just calls `apply` again with reversed parameters.
       * @param input The scrambled string.
       * @return The resulting unscrambled string.
       */
      def unapply(input: String): String

    /**
     * Swaps the characters at two specified positions.
     * @param x The first position index.
     * @param y The second position index.
     */
    private case class SwapPositions(x: Int, y: Int) extends Operation:
      override def apply(input: String): String =
        val (min, max) = (x.min(y), x.max(y))
        val elements: List[String] =
          input.take(min) ::
            input.slice(max, max + 1) ::
            input.slice(min + 1, max) ::
            input.slice(min, min + 1) ::
            input.drop(max + 1) ::
            Nil
        elements.reduce((a, b) => a + b)

      override def unapply(input: String): String =
        this (input)

    /**
     * Swaps the positions of two specified letters.
     * @param x The first character.
     * @param y The second character.
     */
    private case class SwapLetters(x: Char, y: Char) extends Operation:
      override def apply(input: String): String =
        val (xPos, yPos) = (input.indexOf(x), input.indexOf(y))
        SwapPositions(xPos, yPos)(input)

      override def unapply(input: String): String =
        this (input)

    /**
     * Rotates the string left or right by a fixed number of steps.
     * A positive value of x means right rotation, and a negative value means left rotation.
     * @param x The number of steps to rotate.
     */
    private case class Rotate(x: Int) extends Operation:
      override def apply(input: String): String =
        val cutPoint = (2 * input.length - x) % input.length
        val (left, right) = input.splitAt(cutPoint)
        right + left

      /** Reverses a rotation by applying a rotation in the opposite direction. */
      override def unapply(input: String): String =
        Rotate(-x)(input)

    /**
     * Rotates the string based on the current index of a specific character.
     * @param x The character whose position determines the rotation amount.
     */
    private case class RotateByIndex(x: Char) extends Operation:
      override def apply(input: String): String =
        val xPos = input.indexOf(x)
        val rotation = 1 + xPos + (if xPos >= 4 then 1 else 0)
        Rotate(rotation)(input)

      /**
       * Reverses a RotateByIndex operation by calculating the original position
       * of the character 'x' and rotating the string back by the necessary amount.
       */
      override def unapply(input: String): String =
        val newPos = input.indexOf(x)
        val oldPos = if newPos % 2 == 0 then
          val adjustedNewPos = if newPos == 0 then input.length else newPos
          (adjustedNewPos + input.length) / 2 - 1
        else
          (newPos - 1) / 2
        Rotate(newPos - oldPos).unapply(input)

    /**
     * Reverses the order of the characters within a specified sub-segment of the string.
     * @param x The start index of the segment (inclusive).
     * @param y The end index of the segment (inclusive).
     */
    private case class Reverse(x: Int, y: Int) extends Operation:
      override def apply(input: String): String =
        val (min, max) = (x.min(y), x.max(y))
        val elements: List[String] =
          input.take(min) ::
            input.slice(min, max + 1).reverse ::
            input.drop(max + 1) ::
            Nil
        elements.reduce((a, b) => a + b)

      /** The reverse operation is its own inverse. */
      override def unapply(input: String): String =
        this (input)

    /**
     * Moves a character from position x to position y, shifting the intermediate characters.
     * @param x The index of the character to move.
     * @param y The target index where the character should be placed.
     */
    private case class Move(x: Int, y: Int) extends Operation:
      override def apply(input: String): String =
        val reduced = input.take(x) + input.drop(x + 1)
        reduced.take(y) + input(x) + reduced.drop(y)

      /** Reverses a move operation by swapping the source and target indices. */
      override def unapply(input: String): String =
        Move(y, x)(input)

    /**
     * A no-operation, used for parsing errors or unknown commands.
     */
    private case object Nop extends Operation:
      override def apply(input: String): String = input

      override def unapply(input: String): String = input

    /**
     * Companion object for the [[Operation]] trait, providing parsing functionality.
     */
    case object Operation:
      private val swapPositionsRE: Regex = """^swap position (\d+) with position (\d+)$""".r
      private val swapLettersRE: Regex = """^swap letter ([a-z]) with letter ([a-z])$""".r
      private val rotateRE: Regex = """^rotate (left|right) (\d+) steps?$""".r
      private val rotateByIndexRE: Regex = """^rotate based on position of letter ([a-z])$""".r
      private val reverseRE: Regex = """^reverse positions (\d+) through (\d+)$""".r
      private val moveRE: Regex = """^move position (\d+) to position (\d+)$""".r

      /**
       * Attempts to parse an input string into a concrete [[Operation]].
       * @param input The command string (e.g., "swap position 4 with position 0").
       * @return A ZIO task containing the parsed operation, or a successful Nop if parsing fails.
       */
      def parse(input: String): Task[Operation] =
        input match
          case swapPositionsRE(xStr, yStr) =>
            (ZIO.attempt(xStr.toInt) <*> ZIO.attempt(yStr.toInt)).map((x, y) => SwapPositions(x, y))
          case swapLettersRE(xStr, yStr) =>
            ZIO.succeed(SwapLetters(xStr(0), yStr(0)))
          case rotateRE(dirStr, xStr) =>
            ZIO.attempt(xStr.toInt).map(x => if dirStr == "left" then -x else x).map(Rotate(_))
          case rotateByIndexRE(xStr) =>
            ZIO.succeed(RotateByIndex(xStr(0)))
          case reverseRE(xStr, yStr) =>
            (ZIO.attempt(xStr.toInt) <*> ZIO.attempt(yStr.toInt)).map((x, y) => Reverse(x, y))
          case moveRE(xStr, yStr) =>
            (ZIO.attempt(xStr.toInt) <*> ZIO.attempt(yStr.toInt)).map((x, y) => Move(x, y))
          case _ =>
            ZIO.succeed(Nop)

    /**
     * The core service interface for scrambling and unscrambling passwords.
     */
    trait Service:
      /**
       * Applies a sequence of operations to a password string.
       * @param password The initial password.
       * @param operations The sequence of operations to apply.
       * @return A ZIO effect resulting in the final scrambled password.
       */
      def scramble(password: String, operations: Seq[Operation]): ZIO[Any, String, String]

      /**
       * Reverses a sequence of operations to unscramble a password string.
       * The operations are applied in reverse order, using the `unapply` method.
       * @param scrambled The scrambled password.
       * @param operations The sequence of original operations.
       * @return A ZIO effect resulting in the original password.
       */
      def unscramble(scrambled: String, operations: Seq[Operation]): ZIO[Any, String, String]

    /**
     * The live implementation of the Scrambler service.
     */
    val live: ZLayer[Any, Nothing, Scrambler.Service] =
      ZLayer.succeed:
        new Service:
          override def scramble(password: String, operations: Seq[Operation]): ZIO[Any, String, String] =
            operations.foldLeft(ZIO.succeed(password)): (pwdEffect, operation) =>
              for
                pwd <- pwdEffect
                newPwd = operation(pwd)
              yield newPwd

          override def unscramble(scrambled: String, operations: Seq[Operation]): ZIO[Any, String, String] =
            operations.foldRight(ZIO.succeed(scrambled)): (operation, scrambledEffect) =>
              for
                scrambled <- scrambledEffect
                newScrambled = operation.unapply(scrambled)
              yield newScrambled

  /**
   * The main program logic for Day 21.
   */
  private def program =
    val password = "abcdefgh"

    for
      operationStrings <- Util.readStrings("/21.txt")
      operations <- ZIO.foreach(operationStrings)(Operation.parse)
      scrambler <- ZIO.service[Scrambler.Service]

      scrambledPassword <- scrambler.scramble(password, operations)
      _ <- Console.printLine(s"part 1: scrambled password: $scrambledPassword")

      preScrambledPassword = "fbgdceah"
      restoredPassword <- scrambler.unscramble(preScrambledPassword, operations)
      _ <- Console.printLine(s"part 2: unscrambled password: $restoredPassword")
    yield ()

  /**
   * The ZIO application run method, providing the live [[Scrambler.Service]].
   */
  override def run: ZIO[Any, Serializable, Unit] = program.provide(Scrambler.live)