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

  private def longestSubstring(str: String): String =
    @tailrec
    def substrings(str: String, acc: List[String]): List[String] =
      if str.isEmpty then acc
      else
        val index = str.zipWithIndex
          .takeWhile: (c, i) =>
            val haystack = str.take(i)
            !haystack.contains(c)
          .last._2 + 1
        substrings(str.drop(index), str.take(index) :: acc)

    substrings(str, List.empty[String]).maxBy(s => s.length)
  end longestSubstring

  @main def entryPoint(): Unit =
    val firstTest = "abcaa"
    val firstResult = longestSubstring(firstTest)
    println(firstResult)

    val secondTest = "aacba"
    val secondResult = longestSubstring(secondTest)
    println(secondResult)

    val thirdTest = "12345"
    val reversed = thirdTest.reversed
    println(reversed)

