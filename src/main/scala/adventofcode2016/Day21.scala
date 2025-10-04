package adventofcode2016

import adventofcode2016.Day21.Scrambler.Operation
import zio.{Console, Task, ZIO, ZIOAppDefault, ZLayer}

import scala.util.matching.Regex

object Day21 extends ZIOAppDefault:

  object Scrambler:
    sealed trait Operation:
      def apply(input: String): String

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

    private case class SwapLetters(x: Char, y: Char) extends Operation:
      override def apply(input: String): String =
        val (xPos, yPos) = (input.indexOf(x), input.indexOf(y))
        SwapPositions(xPos, yPos)(input)

    private case class Rotate(x: Int) extends Operation:
      override def apply(input: String): String =
        val cutPoint = (2 * input.length - x) % input.length
        val (left, right) = input.splitAt(cutPoint)
        right + left

    private case class RotateByIndex(x: Char) extends Operation:
      override def apply(input: String): String =
        val xPos = input.indexOf(x)
        val rotation = 1 + xPos + (if xPos >= 4 then 1 else 0)
        Rotate(rotation)(input)

    private case class Reverse(x: Int, y: Int) extends Operation:
      override def apply(input: String): String =
        val (min, max) = (x.min(y), x.max(y))
        val elements: List[String] =
          input.take(min) ::
            input.slice(min, max + 1).reverse ::
            input.drop(max + 1) ::
            Nil
        elements.reduce((a, b) => a + b)

    private case class Move(x: Int, y: Int) extends Operation:
      override def apply(input: String): String =
        val reduced = input.take(x) + input.drop(x + 1)
        reduced.take(y) + input(x) + reduced.drop(y)

    private case object Nop extends Operation:
      override def apply(input: String): String = input

    case object Operation:
      private val swapPositionsRE: Regex = """^swap position (\d+) with position (\d+)$""".r
      private val swapLettersRE: Regex = """^swap letter ([a-z]) with letter ([a-z])$""".r
      private val rotateRE: Regex = """^rotate (left|right) (\d+) steps?$""".r
      private val rotateByIndexRE: Regex = """^rotate based on position of letter ([a-z])$""".r
      private val reverseRE: Regex = """^reverse positions (\d+) through (\d+)$""".r
      private val moveRE: Regex = """^move position (\d+) to position (\d+)$""".r

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

    trait Service:
      def scramblePassword(password: String, operations: Seq[Operation]): ZIO[Any, String, String]

    val live: ZLayer[Any, Nothing, Scrambler.Service] =
      ZLayer.succeed:
        new Service:
          override def scramblePassword(password: String, operations: Seq[Operation]): ZIO[Any, String, String] =
            operations.foldLeft(ZIO.succeed(password)): (pwdEffect, operation) =>
              for
                pwd <- pwdEffect
                newPwd = operation(pwd)
              yield newPwd

  private def program =
    val password = "abcdefgh"

    for
      operationStrings <- Util.readStrings("/21.txt")
      operations <- ZIO.foreach(operationStrings)(Operation.parse)
      scrambler <- ZIO.service[Scrambler.Service]
      scrambledPassword <- scrambler.scramblePassword(password, operations)
      _ <- Console.printLine(s"part 1: scrambled password: $scrambledPassword")
    yield ()

  override def run: ZIO[Any, Serializable, Unit] = program.provide(Scrambler.live)