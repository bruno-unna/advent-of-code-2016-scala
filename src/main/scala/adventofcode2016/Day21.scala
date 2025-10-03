package adventofcode2016

import zio.{Console, Task, ZIO, ZIOAppDefault, ZLayer}

import java.io.IOException
import scala.util.matching.Regex

object Day21 extends ZIOAppDefault:

  object Scrambler:
    sealed trait Operation:
      def apply(input: String): String

    case class SwapPositions(x: Int, y: Int) extends Operation:
      override def apply(input: String): String = ???

    case class SwapLetters(c: Char, c1: Char) extends Operation:
      override def apply(input: String): String = ???

    case class Rotate(x: Int) extends Operation:
      override def apply(input: String): String = ???

    case class RotateByIndex(x: Char) extends Operation:
      override def apply(input: String): String = ???

    case class Reverse(x: Int, y: Int) extends Operation:
      override def apply(input: String): String = ???

    case class Move(x: Int, y: Int) extends Operation:
      override def apply(input: String): String = ???

    case object Operation:
      private val swapPositionsRE: Regex = """^swap position (\d+) with position (\d+)$""".r
      private val swapLettersRE: Regex = """^swap letter ([a-z]) with letter ([a-z])$""".r
      private val rotateRE: Regex = """^rotate (left|right) (\d+) steps$""".r
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

    trait Service:
      def scramblePassword(password: String): ZIO[Any, String, String]

    val live: ZLayer[Any, Nothing, Scrambler.Service] =
      ZLayer.succeed:
        new Service:
          override def scramblePassword(password: String): ZIO[Any, String, String] =
            ???

  private def program =
    val password = "abcdefgh"

    for
      operations <- Util.readStrings("/21.txt")
      scrambler <- ZIO.service[Scrambler.Service]
      scrambledPassword <- scrambler.scramblePassword(password)
      _ <- Console.printLine(s"part 1: scrambled password: $scrambledPassword")
    yield ()

  override def run: ZIO[Any, IOException | String, Unit] = program.provide(Scrambler.live)