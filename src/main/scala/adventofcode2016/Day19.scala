package adventofcode2016

object Day19:
  def reduce(elves: Int): Int = ???

  @main
  def day19(): Unit =
    val elves = 3004953

    val winner = reduce(elves)
    println(s"Part 1: out of $elves elves, the winner is $winner")
