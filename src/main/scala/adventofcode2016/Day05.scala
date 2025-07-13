package adventofcode2016

import adventofcode2016.Util

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.annotation.tailrec

object Day05 extends App:

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

  def calculateSecondPassword(doorID: String): String =
    @tailrec
    def collectChars(
        n: Int,
        acc: List[(Char, Char)],
        seen: Set[Char]
    ): List[(Char, Char)] =
      if acc.length >= 8 then acc
      else
        val hash = md5(doorID + n)
        if hash.startsWith("00000") then
          val pos = hash.charAt(5)
          val chr = hash.charAt(6)
          if pos >= '0' && pos <= '7' && !seen.contains(pos) then
            collectChars(n + 1, (pos, chr) :: acc, seen + pos)
          else collectChars(n + 1, acc, seen)
        else collectChars(n + 1, acc, seen)

    collectChars(0, List.empty[(Char, Char)], Set.empty[Char])
      .sortBy: (pos, chr) =>
        pos
      .map(_._2)
      .mkString

  @main def run =
    println("Day 05")
    val doorID = "ugkcyxxp"
    val password = calculatePassword(doorID)
    println(s"password: $password")
    val secondPassword = calculateSecondPassword(doorID)
    println(s"second password: $secondPassword")
