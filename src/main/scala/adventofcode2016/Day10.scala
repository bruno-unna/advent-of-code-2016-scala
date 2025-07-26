package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.util.matching.Regex

case class Factory():
  def execute(): UIO[Unit] = ???

  def findComparingBot(chipA: Int, chipB: Int): UIO[Int] = ???

object Factory:
  def make(instructions: Seq[String]): UIO[Factory] = ???

object Day10 extends ZIOAppDefault:
  val MarkerDataRE: Regex = """^(.*?)\((\d+)x(\d+)\)(.*)$""".r

  def run =
    for
      instructions <- Util.readStrings("/10.txt")

      factory <- Factory.make(instructions)
      _ <- factory.execute()

      foundBot <- factory.findComparingBot(5, 2)

      _ <- Console.printLine(
        s"part 1: the bot comparing chips 61 and 17 is ${foundBot}"
      )
    yield ExitCode.success
