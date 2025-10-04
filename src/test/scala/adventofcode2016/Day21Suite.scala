package adventofcode2016

import adventofcode2016.Day21.Scrambler
import adventofcode2016.Day21.Scrambler.Operation
import zio.ZIO
import zio.test.{Spec, ZIOSpecDefault, assertTrue}

object Day21Suite extends ZIOSpecDefault:

  val operationStrings: Seq[String] =
    "swap position 4 with position 0" ::
      "swap letter d with letter b" ::
      "reverse positions 0 through 4" ::
      "rotate left 1 step" ::
      "move position 1 to position 4" ::
      "move position 3 to position 0" ::
      "rotate based on position of letter b" ::
      "rotate based on position of letter d" ::
      Nil
  val password = "abcde"
  val scrambledPassword = "decab"

  def spec: Spec[Any, String | Throwable] = suite("password scrambling")(
    test("test scrambling"):
      for
        scrambler <- ZIO.service[Scrambler.Service]
        operations <- ZIO.foreach(operationStrings)(Operation.parse)
        actualScrambledPwd <- scrambler.scramble(password, operations)
        test <- assertTrue(actualScrambledPwd == scrambledPassword)
      yield test
  ).provide(Scrambler.live)
