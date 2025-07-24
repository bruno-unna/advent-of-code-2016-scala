package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.util.matching.Regex

case class Display(
    width: Int,
    height: Int,
    private val displayRef: Ref[List[List[Boolean]]]
):
  displayRef.update: _ =>
    List.fill(height, width)(false)

  def countLeds: UIO[Int] =
    displayRef.get.map: m =>
      m.reduce(_ ::: _).count(_ == true)

  def execute(p: Seq[String]): UIO[Display] = ???

object Display:
  def make(width: Int, height: Int) =
    val initialDisplay = List.fill(height, width)(false)
    Ref
      .make(initialDisplay)
      .map:
        Display(width, height, _)

object Day08 extends ZIOAppDefault:
  val SCREEN_WIDTH = 50
  val SCREEN_HEIGHT = 6

  def run =
    for
      program <- Util.readStrings("/08.txt")
      display <- Display.make(SCREEN_WIDTH, SCREEN_HEIGHT)
      newDisplay <- display.execute(program)
      nLeds <- newDisplay.countLeds
      _ <- Console.print(
        s"Day 08\n\tnumber of leds on after the program: ${nLeds}\n"
      )
    yield ExitCode.success
