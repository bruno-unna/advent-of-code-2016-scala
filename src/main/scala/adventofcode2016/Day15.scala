package adventofcode2016

object Day15 extends App:

  case class Disc(n: Int, positions: Int, offset: Int):
    def openAtTime(t: Int): Boolean = (n + offset + t) % positions == 0

  case class Machine(discs: Seq[Disc]):
    private def openAtTime(t: Int): Boolean = discs.forall(_.openAtTime(t))

    def calculateRightTime(): Int =
      val timeStream = LazyList.from(0)
      timeStream.find(this.openAtTime).get

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
