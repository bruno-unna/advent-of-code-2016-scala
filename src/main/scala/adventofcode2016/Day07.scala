package adventofcode2016

import adventofcode2016.Util
import zio.*
import scala.util.matching.Regex

object Day07 extends ZIOAppDefault:

  val ipBlockRE: Regex = """(?:([a-z]+)(?:\[([a-z]+)\])?)""".r
  val abbaRE: Regex = """[a-z]*([a-z])((?!\1)[a-z])\2\1[a-z]*""".r
  val abaRE: Regex = """([a-z])((?!\1)[a-z])\1""".r

  def supportsTLS(ip: String): Boolean =
    val segments = ipBlockRE
      .findAllMatchIn(ip)
      .map: m =>
        val out = Option(m.group(1)).map(abbaRE.matches(_)).getOrElse(false)
        val in = Option(m.group(2)).map(abbaRE.matches(_)).getOrElse(false)
        (out, in)
      .toList
    segments.map(_._1).exists(_ == true) &&
    segments.map(_._2).forall(_ == false)

  def supportsSSL(ip: String): Boolean =
    val segments = ipBlockRE
      .findAllMatchIn(ip)
      .map: m =>
        (Option(m.group(1)), Option(m.group(2)))
      .toList
    val supernet = segments
      .map(_._1)
      .flatten
      .mkString(" ")
    val hypernet = segments
      .map(_._2)
      .flatten
      .mkString(" ")
    val matchFound = (0 to supernet.length - 3)
      .map: index =>
        val subSupernet = supernet.substring(index)
        val maybeBab = abaRE
          .findFirstIn(subSupernet)
          .map: aba =>
            List(aba.charAt(1), aba.charAt(0), aba.charAt(1)).mkString
        maybeBab.map(hypernet.contains(_))
      .flatten
    matchFound.exists(_ == true)

  def run =
    for
      ips <- Util.readStrings("/07.txt")
      tlsSupportCount = ips.count(supportsTLS)
      sslSupportCount = ips.count(supportsSSL)
      _ = printf(
        "Day 07\n\tnumber of IP addresses supporting TLS: %d\n\tnumber of IP addresses supporting SSL: %d\n",
        tlsSupportCount,
        sslSupportCount
      )
    yield ExitCode.success
