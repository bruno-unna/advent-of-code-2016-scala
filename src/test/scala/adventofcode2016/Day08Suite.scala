package adventofcode2016

import adventofcode2016.Day08.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object Day08Suite extends ZIOSpecDefault:
  val SCREEN_WIDTH = 7
  val SCREEN_HEIGHT = 3

  def spec: Spec[Any, Throwable] = suite("Display Unit Tests")(
    test("display programming should leave correct number of leds on"):
      val program: List[String] = List(
        "rect 3x2",
        "rotate column x=1 by 1",
        "rotate row y=0 by 4",
        "rotate column x=1 by 1"
      )
      for
        display <- Display.make(SCREEN_WIDTH, SCREEN_HEIGHT)
        newDisplay <- display.execute(program)
        nLeds <- newDisplay.countLeds
      yield assertTrue(nLeds == 6)
  )
