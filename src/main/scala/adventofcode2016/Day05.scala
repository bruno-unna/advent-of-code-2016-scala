package adventofcode2016

import zio._
import adventofcode2016.Util
import java.security.MessageDigest
import java.nio.charset.StandardCharsets

object Day05:

  val md = MessageDigest.getInstance("MD5")

  private def md5(s: String): String =
    val hashBytes = md.digest(s.getBytes(StandardCharsets.UTF_8))
    hashBytes.map("%02x".format(_)).mkString

  def calculatePassword(doorID: String): String =
    ???

  @main def run =
    val doorID = "ugkcyxxp"
    val password = calculatePassword(doorID)
    printf(
      "Day 05\n\tpassword: %s\n",
      password
    )
