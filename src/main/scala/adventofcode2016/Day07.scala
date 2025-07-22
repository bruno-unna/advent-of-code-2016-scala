package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.util.matching.Regex

/** Solves Day 7 of Advent of Code 2016, which involves analysing IP addresses
  * to determine if they support Transport Layer Snooping (TLS) or Super-Secret
  * Listening (SSL).
  */
object Day07 extends ZIOAppDefault:

  /** Regular expression to parse IP address segments. It captures a sequence of
    * lowercase letters (supernet sequence) optionally followed by a bracketed
    * sequence of lowercase letters (hypernet sequence).
    */
  val ipBlockRE: Regex = """(?:([a-z]+)(?:\[([a-z]+)\])?)""".r

  /** Regular expression to detect an ABBA sequence. An ABBA is any
    * four-character sequence which consists of a pair of two different
    * characters followed by the reverse of that pair, such as `abba` or `bddb`.
    */
  val abbaRE: Regex = """[a-z]*([a-z])((?!\1)[a-z])\2\1[a-z]*""".r

  /** Regular expression to detect an ABA (Area-Broadcast Accessor) sequence. An
    * ABA is any three-character sequence which consists of a pair of two
    * different characters followed by the first character, such as `aba` or
    * `xyx`.
    */
  val abaRE: Regex = """([a-z])((?!\1)[a-z])\1""".r

  /** Determines if an IP address supports Transport Layer Snooping (TLS). An IP
    * supports TLS if it has at least one ABBA sequence in any supernet
    * sequence, but no ABBA sequence in any hypernet sequence.
    *
    * @param ip
    *   The IP address string to check.
    * @return
    *   `true` if the IP address supports TLS, `false` otherwise.
    */
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

  /** Determines if an IP address supports Super-Secret Listening (SSL). An IP
    * supports SSL if it contains an Area-Broadcast Accessor (ABA) in any
    * supernet sequence, and that ABA corresponds to a BAB (Byte Allocation
    * Block) in any hypernet sequence. A BAB is the inverse of an ABA (e.g., if
    * ABA is `xyx`, its corresponding BAB is `yxy`).
    *
    * @param ip
    *   The IP address string to check.
    * @return
    *   `true` if the IP address supports SSL, `false` otherwise.
    */
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
            s"${aba.charAt(1)}${aba.charAt(0)}${aba.charAt(1)}"
        maybeBab.map(hypernet.contains(_))
      .flatten
    matchFound.exists(_ == true)

  /** The main entry point for the ZIO application. Reads IP addresses from a
    * file, counts those supporting TLS and SSL, and prints the results to the
    * console.
    *
    * @return
    *   A ZIO effect that runs the application and exits with success.
    */
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
