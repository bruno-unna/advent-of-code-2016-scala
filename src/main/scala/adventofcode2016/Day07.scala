package adventofcode2016

import adventofcode2016.Util
import zio.*

object Day07 extends ZIOAppDefault:

  def supportsTLS(ip: String): Boolean = ???

  def run =
    for
      ips <- Util.readStrings("/07.txt")
      tlsSupportCount = ips.count(supportsTLS)
      _ = printf(
        "Day 07\n\tnumber of IP addresses supporting TLS: %d\n",
        tlsSupportCount
      )
    yield ExitCode.success
