package adventofcode2016

import scala.annotation.tailrec

object SiriusXM extends App:

  extension (s: String)
    private def reversed: String =
      @tailrec
      def loop(s: String, acc: String): String =
        if s.isEmpty then acc
        else
          loop(s.drop(1), s.charAt(0) +: acc)

      loop(s, "")

    private def longestSubstring: String =
      def findSubStr(s: String, length: Int): Option[String] =
        val subStrings = for
          indexes <- 0 to s.length - length
          subStr = s.substring(indexes, indexes + length)
        yield subStr
        subStrings.find(subStr => subStr.toCharArray.distinct.length == subStr.length)

      val lengthOfWindow = Range(s.length, 0, -1)
      val subStrings = lengthOfWindow.map: l =>
        findSubStr(s, l)

      subStrings.collectFirst:
        case Some(s) => s
      .get

  @main def entryPoint(): Unit =
    val firstTest = "abcaa"
    val firstResult = firstTest.longestSubstring
    println(firstResult)

    val secondTest = "aacba"
    val secondResult = secondTest.longestSubstring
    println(secondResult)

    val thirdTest = "12345"
    val reversed = thirdTest.reversed
    println(reversed)

