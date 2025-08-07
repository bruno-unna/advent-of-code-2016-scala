package adventofcode2016

import zio.test.{Spec, ZIOSpecDefault, assertTrue}

object Day05ZIOSuite extends ZIOSpecDefault:

  def spec: Spec[Any, Throwable] = suite("Password cracking")(
    test("calculate the password"):
      val doorID = "abc"
      for password <- Day05ZIO.calculatePassword(doorID)
      yield assertTrue(password == "18f47a30")
    ,
    test("calculate the second password"):
      val doorID = "abc"
      for password <- Day05ZIO.calculateSecondPassword(doorID)
      yield assertTrue(password == "05ace8e3")
  )
