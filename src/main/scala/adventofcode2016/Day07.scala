package adventofcode2016

import adventofcode2016.Util
import zio.*
import scala.util.matching.Regex

object Day07 extends ZIOAppDefault:

  val abbaRE: Regex = """[a-z]*([a-z])((?!\1)[a-z])\2\1[a-z]*""".r
  val ipBlockRE: Regex = """(?:([a-z]+)(?:\[([a-z]+)\])?)""".r

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

  def run =
    for
      ips <- Util.readStrings("/07.txt")
      tlsSupportCount = ips.count(supportsTLS)
      _ = printf(
        "Day 07\n\tnumber of IP addresses supporting TLS: %d\n",
        tlsSupportCount
      )
    yield ExitCode.success
