package adventofcode2016

import zio.{Console, Task, ZIO, ZIOAppDefault, ZLayer}

import java.io.IOException

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