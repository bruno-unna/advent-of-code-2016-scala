package adventofcode2016

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day17:

  private def newMd5(): MessageDigest = MessageDigest.getInstance("MD5")

  case class Coordinates(x: Int, y: Int):
    def checkMobility(s: String): Set[(Char, Coordinates)] =
      val md = newMd5()
      val hashBytes = md.digest(s.getBytes(StandardCharsets.UTF_8))
      val dirs = Array('U', 'D', 'L', 'R')
      val availability = hashBytes.map("%02x".format(_)).mkString.take(4).toCharArray.map:
        case 'b' | 'c' | 'd' | 'e' | 'f' => true
        case _ => false
      dirs.zip(availability).map:
        case ('U', true) if y > 0 => Some(('U', Coordinates(x, y - 1)))
        case ('D', true) if y < 3 => Some(('D', Coordinates(x, y + 1)))
        case ('L', true) if x > 0 => Some(('L', Coordinates(x - 1, y)))
        case ('R', true) if x < 3 => Some(('R', Coordinates(x + 1, y)))
        case (_, _) => None
      .collect:
        case Some(v) => v
      .toSet

  def findSteps(passcode: String): String =
    val origin = Coordinates(0, 0)
    val destination = Coordinates(3, 3)
    val initialPath = ""
    val initialQueue = Queue((origin, ""))

    @tailrec
    def loop(path: String, queue: Queue[(Coordinates, String)]): String =
      if queue.isEmpty then path
      else
        val ((current, path), reducedQueue) = queue.dequeue
        if current == destination then path
        else
          val availableMoves = current.checkMobility(passcode + path)
          val newQueue = availableMoves.foldLeft(reducedQueue)((q, m) => q.enqueue(m._2, path + m._1))
          loop(path, newQueue)

    loop(initialPath, initialQueue)

  def findAllPaths(passcode: String): Set[String] =
    val origin = Coordinates(0, 0)
    val destination = Coordinates(3, 3)
    val initialPath = ""

    def loop(path: String, cell: Coordinates): Set[String] =
      if cell == destination then Set(path)
      else
        val availableMoves = cell.checkMobility(passcode + path)
        for
          (direction, coordinates) <- availableMoves
          newCells <- loop(path + direction, coordinates)
        yield newCells

    val availableMoves = origin.checkMobility(passcode)
    availableMoves.flatMap: (direction, coordinates) =>
      loop(initialPath + direction, coordinates)

  @main
  def day17(): Unit =
    val passcode = "qljzarfv"

    val steps = findSteps(passcode)
    println(s"Part 1: the minimum path is $steps")

    val allPaths = findAllPaths(passcode)
    val maxLength = allPaths.map(_.length).max
    println(s"Part 2: the maximum path length is $maxLength")
