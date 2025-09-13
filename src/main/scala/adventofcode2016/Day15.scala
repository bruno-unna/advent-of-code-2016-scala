package adventofcode2016

/**
 * Solves Advent of Code 2016, Day 15.
 */
object Day15 extends App:

  /**
   * Represents a single disc in the capsule machine.
   *
   * @param n The number of the disc, starting from 1.
   * @param positions The total number of positions the disc can be in.
   * @param offset The initial position of the disc at time t=0.
   */
  private case class Disc(n: Int, positions: Int, offset: Int):
    /**
     * Checks if the disc is in the "open" position at a given time.
     * A disc is open if its final position (n + offset + t) is a multiple of its total positions.
     *
     * @param t The time in seconds since the capsule was released.
     * @return true if the disc is open, false otherwise.
     */
    def openAtTime(t: Int): Boolean = (n + offset + t) % positions == 0

  /**
   * Represents the capsule machine with a sequence of discs.
   *
   * @param discs The sequence of discs in the machine.
   */
  private case class Machine(discs: Seq[Disc]):
    /**
     * Checks if all discs in the machine are open at a given time.
     *
     * @param t The time in seconds.
     * @return true if all discs are open, false otherwise.
     */
    private def openAtTime(t: Int): Boolean = discs.forall(_.openAtTime(t))

    /**
     * Finds the first non-negative time `t` at which all discs are in the open position.
     *
     * This is a brute-force approach that checks each time step starting from 0.
     *
     * @return The earliest time at which all discs are open.
     */
    def calculateRightTime(): Int =
      val timeStream = LazyList.from(0)
      timeStream.find(this.openAtTime).get

  /**
   * Main entry point for the Day 15 solution.
   */
  @main def runDay15(): Unit =
    println("Day 15")

    val firstMachine = Machine(Array(
      Disc(n = 1, positions = 17, offset = 1),
      Disc(n = 2, positions = 7, offset = 0),
      Disc(n = 3, positions = 19, offset = 2),
      Disc(n = 4, positions = 5, offset = 0),
      Disc(n = 5, positions = 3, offset = 0),
      Disc(n = 6, positions = 13, offset = 5),
    ))

    val firstTime = firstMachine.calculateRightTime()
    println(s"time to push the button for the first capsule: $firstTime")

    val secondMachine = Machine(Array(
      Disc(n = 1, positions = 17, offset = 1),
      Disc(n = 2, positions = 7, offset = 0),
      Disc(n = 3, positions = 19, offset = 2),
      Disc(n = 4, positions = 5, offset = 0),
      Disc(n = 5, positions = 3, offset = 0),
      Disc(n = 6, positions = 13, offset = 5),
      Disc(n = 7, positions = 11, offset = 0),
    ))

    val secondTime = secondMachine.calculateRightTime()
    println(s"time to push the button for the second capsule: $secondTime")