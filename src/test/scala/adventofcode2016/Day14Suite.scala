package adventofcode2016

import zio.test.{Spec, ZIOSpecDefault, assertTrue}

object Day14Suite extends ZIOSpecDefault:

  def spec: Spec[Any, Throwable] = suite("OTP generation")(
    test("assert correct values"):
      val salt = "abc"
      for
        otp <- Day14.calculateOTP(salt)
        test <- assertTrue(otp(0) == 39, otp(1) == 92, otp(63) == 22728)
      yield test
  )
