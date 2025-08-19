package adventofcode2016

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day13:

  case class Coordinates(x: Int, y: Int):
    def isSpace(number: Int): Boolean =
      val n = number + x * x + 3 * x + 2 * x * y + y + y * y
      val nBits = n.toBinaryString.count(_ == '1')
      nBits % 2 == 0

    def isWall(number: Int): Boolean = !isSpace(number)

  def reconstructPath(parents: Map[Coordinates, Coordinates], target: Coordinates): List[Coordinates] =
    @tailrec
    def loop(current: Coordinates, acc: List[Coordinates]): List[Coordinates] =
      parents.get(current) match
        case Some(parent) => loop(parent, current :: acc)
        case None => current :: acc

    loop(target, Nil)

  def findSteps(origin: Coordinates, destination: Coordinates, favouriteNumber: Int): List[Coordinates] =

    @tailrec
    def loop(parents: Map[Coordinates, Coordinates], seen: Set[Coordinates], queue: Queue[Coordinates]): Map[Coordinates, Coordinates] =
      if queue.isEmpty then parents
      else
        val (current, reducedQueue) = queue.dequeue
        if current == destination then parents
        else
          val candidates = Set(
            Coordinates(current.x - 1, current.y),
            Coordinates(current.x + 1, current.y),
            Coordinates(current.x, current.y - 1),
            Coordinates(current.x, current.y + 1),
          ).filterNot(c => c.x < 0 || c.y < 0).diff(seen)
          val (validCandidates, walls) = candidates.partition(_.isSpace(favouriteNumber))
          val newSeen = seen ++ walls + current
          val newQueue = reducedQueue.enqueueAll(validCandidates)
          val newPredecessors = parents ++ validCandidates.map(_ -> current)
          loop(newPredecessors, newSeen, newQueue)

    val predecessorMap = loop(Map.empty, Set(origin), Queue(origin))
    reconstructPath(predecessorMap, destination)

  def visitedLocations(origin: Coordinates, favouriteNumber: Int, maxSteps: Int): Set[Coordinates] =
    @tailrec
    def loop(seen: Set[Coordinates], queue: Queue[(Coordinates, Int)]): Set[Coordinates] =
      if queue.isEmpty then seen
      else
        val ((current, steps), reducedQueue) = queue.dequeue
        if steps >= maxSteps then seen
        else
          val candidates = Set(
            Coordinates(current.x - 1, current.y),
            Coordinates(current.x + 1, current.y),
            Coordinates(current.x, current.y - 1),
            Coordinates(current.x, current.y + 1),
          ).filterNot(c => c.x < 0 || c.y < 0).filter(_.isSpace(favouriteNumber))

          val newCandidates = candidates.diff(seen)
          val newQueue = reducedQueue.enqueueAll(newCandidates.map(_ -> (steps + 1)))
          loop(seen ++ newCandidates, newQueue)

    val initialQueue = Queue(origin -> 0)
    val initialSeen = Set(origin)
    loop(initialSeen, initialQueue)


  @main
  def day13(): Unit =
    val favouriteNumber = 1362

    val steps = findSteps(Coordinates(1, 1), Coordinates(31, 39), favouriteNumber)
    println(s"Part 1: the minimum path length is ${steps.length - 1}")

    val maxSteps = 50

    val locations = visitedLocations(Coordinates(1, 1), favouriteNumber, maxSteps)
    println(s"Part 2: visitable locations within $maxSteps steps: ${locations.size}")