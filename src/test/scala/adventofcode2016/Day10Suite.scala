package adventofcode2016

import adventofcode2016.Day10.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object Day10Suite extends ZIOSpecDefault:
  def spec: Spec[Any, Throwable] = suite("Balance Bots Unit Tests")(
    test("small pass of chips"):
      val instructions: List[String] = List(
        "value 5 goes to bot 2",
        "bot 2 gives low to bot 1 and high to bot 0",
        "value 3 goes to bot 1",
        "bot 1 gives low to output 1 and high to bot 0",
        "bot 0 gives low to output 2 and high to output 0",
        "value 2 goes to bot 2"
      )
      for
        factory <- Factory.make(instructions)
        _ <- factory.execute()

        foundBot <- factory.findComparingBot(5, 2)
      yield assertTrue(foundBot == 2)
  )
