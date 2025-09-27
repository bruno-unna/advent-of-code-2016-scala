package adventofcode2016

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * Solves Advent of Code Day 17: Two Steps Forward.
 *
 * This problem involves navigating a 4x4 grid using a path determined by MD5 hashes.
 */
object Day17:

  /**
   * Creates a new instance of the MD5 [[MessageDigest]].
   *
   * Note that this is a method instead of a single static value because
   * the `java.security.MessageDigest` implementation is not thread safe.
   *
   * @return A new [[MessageDigest]] instance configured for MD5.
   */
  private def newMd5(): MessageDigest = MessageDigest.getInstance("MD5")

  /**
   * Represents a position on the 4x4 grid.
   *
   * The grid coordinates are (0, 0) in the top-left and (3, 3) in the bottom-right.
   *
   * @param x The column index (0-3).
   * @param y The row index (0-3).
   */
  case class Coordinates(x: Int, y: Int):
    /**
     * Determines the available moves from the current coordinates based on the given path string.
     *
     * The path string is hashed (passcode + current path) and the first four hex characters
     * determine which doors are open: U, D, L, R respectively. A character 'b' through 'f'
     * indicates an open door.
     *
     * @param s The string to hash, usually the passcode followed by the path taken so far.
     * @return A [[Set]] of possible moves, where each element is a tuple of the direction
     *         character ('U', 'D', 'L', 'R') and the resulting [[Coordinates]].
     */
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

  /**
   * Finds the shortest path (minimum steps) from (0, 0) to (3, 3).
   *
   * This uses a Breadth-First Search (BFS) approach, which guarantees the first path
   * found to the destination is the shortest.
   *
   * @param passcode The starting passcode string.
   * @return The path string (sequence of 'U', 'D', 'L', 'R' characters) for the shortest path.
   */
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

  /**
   * Finds all possible paths from (0, 0) to (3, 3).
   *
   * This uses a Depth-First Search (DFS) approach to explore every possible path.
   *
   * @param passcode The starting passcode string.
   * @return A [[Set]] containing all possible path strings that reach the destination.
   */
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

  /**
   * The main entry point for Day 17.
   *
   * Calculates and prints the shortest path (Part 1) and the length of the longest path (Part 2).
   */
  @main
  def day17(): Unit =
    val passcode = "qljzarfv"

    val steps = findSteps(passcode)
    println(s"Part 1: the minimum path is $steps")

    val allPaths = findAllPaths(passcode)
    val maxLength = allPaths.map(_.length).max
    println(s"Part 2: the maximum path length is $maxLength")