package adventofcode2016

import adventofcode2016.Day14.Hasher
import zio.test.{Spec, ZIOSpecDefault, assertTrue}

object Day14Suite extends ZIOSpecDefault:

  def spec: Spec[Any, Throwable] = suite("OTP generation")(
    test("test ordinary hashes"):
      val salt = "abc"
      for
        otp <- Day14.calculateOTP(salt)
        test <- assertTrue(otp(0)._1 == 39, otp(1)._1 == 92, otp(64)._1 == 22728)
      yield test
    ,
    test("test stretched hashes"):
      val salt = "abc"
      for
        otp <- Day14.calculateOTP(salt, stretched = true)
        test <- assertTrue(otp(0)._1 == 10, otp(64)._1 == 22551)
      yield test
  ).provide(Hasher.live)
