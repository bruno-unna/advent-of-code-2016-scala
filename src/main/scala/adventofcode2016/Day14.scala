package adventofcode2016

import zio.*

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object Day14 extends ZIOAppDefault:

  /** Creates a new MD5 MessageDigest instance for thread safety.
    */
  private def newMd5(): MessageDigest = MessageDigest.getInstance("MD5")

  /** Calculates the MD5 hash of a given string.
    *
    * @param s
    *   The input string.
    * @return
    *   The MD5 hash of the string as a 32-character hexadecimal string.
    */
  private def md5(s: String): UIO[String] =
    ZIO.succeed:
      val md = newMd5()
      val hashBytes = md.digest(s.getBytes(StandardCharsets.UTF_8))
      hashBytes.map("%02x".format(_)).mkString

  def calculateOTP(str: String): UIO[Vector[Int]] = ???

  def run: ZIO[ZIOAppArgs & Scope, IOException, Unit] =
    val salt = "ihaygndm"

    for
      otp <- calculateOTP(salt)
      _ <- Console.printLine(s"index for 64th key: ${otp(63)}")
    yield ()
