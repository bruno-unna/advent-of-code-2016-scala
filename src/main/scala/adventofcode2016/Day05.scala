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
    val naturalNumbers: LazyList[Int] = LazyList.from(0)
    val maybeChars = naturalNumbers.map: n =>
      val hash = md5(doorID + n)
      if hash.startsWith("00000") then Some(hash.charAt(5))
      else None
    val goodChars = maybeChars.collect:
      case Some(c) => c
    goodChars.take(8).mkString

  @main def run =
    val doorID = "ugkcyxxp"
    val password = calculatePassword(doorID)
    printf(
      "Day 05\n\tpassword: %s\n",
      password
    )
