package adventofcode2016

import zio._
import adventofcode2016.Util

object Day05:

  def calculatePassword(doorID: String): String = ???

  @main def run =
    val doorID = "ugkcyxxp"
    val password = calculatePassword(doorID)
    printf(
      "Day 05\n\tpassword: %s\n",
      password
    )
