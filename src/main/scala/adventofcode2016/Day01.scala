package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.matching.Regex

/** Represents the cardinal directions.
  */
enum Heading:
  case North, South, East, West

/** Represents a rotation direction.
  */
enum Rotation:
  case Left, Right

/** Represents a two-dimensional position with a current heading.
  *
  * @param x
  *   The x-coordinate.
  * @param y
  *   The y-coordinate.
  * @param heading
  *   The current direction the entity is facing.
  */
case class Position(x: Int, y: Int, heading: Heading):
  /** Calculates the Manhattan distance (L1 norm) from this position to another.
    *
    * @param there
    *   The target position.
    * @return
    *   The Manhattan distance between the two positions.
    */
  def distanceTo(there: Position): Int =
    Math.abs(x - there.x) + Math.abs(y - there.y)

/** Represents a single movement transition, consisting of a rotation and a
  * distance.
  *
  * @param rotation
  *   The direction of rotation (Left or Right).
  * @param distance
  *   The number of units to move after rotation.
  */
case class Transition(rotation: Rotation, distance: Int)

/** Regular expression to parse a single transition string (e.g., "R5", "L10").
  * It captures the rotation direction (R or L) and the distance (digits).
  */
val TransitionRE: Regex = """\s*(R|L)(\d+)\s*""".r

/** Provides extension methods for `String` to parse navigation instructions.
  */
extension (s: String)
  /** Attempts to convert a string (e.g., "R5", "L10") into a [[Transition]]
    * object.
    *
    * It uses the [[TransitionRE]] regex to parse the string. If parsing is
    * successful, it returns `Some(Transition)`. If the string does not match
    * the expected format or the distance is not a valid integer, it returns
    * `None`.
    *
    * @return
    *   An `Option` containing the parsed `Transition` if successful, otherwise
    *   `None`.
    */
  def toTransition: Option[Transition] =
    s match
      case TransitionRE(rot, dist) =>
        Some(
          Transition(
            if rot == "R" then Rotation.Right else Rotation.Left,
            dist.toIntOption.getOrElse(0)
          )
        )
      case _ => None

  /** Converts a comma-separated string of transitions (e.g., "R5, L2, R10")
    * into a `List` of [[Transition]] objects.
    *
    * It splits the string by commas, attempts to parse each segment using
    * [[toTransition]], and filters out any segments that cannot be successfully
    * parsed.
    *
    * @return
    *   A `List` of parsed [[Transition]] objects.
    */
  def toTrajectory: List[Transition] =
    s.split(',')
      .map(_.toTransition)
      .filter(_.isDefined)
      .map(_.get)
      .toList

/** Provides an extension method for `List[Position]` to find the first
  * revisited position.
  */
extension (history: List[Position])
  /** Finds the first position in the history list that is revisited (i.e.,
    * appears at least twice in the list).
    *
    * The comparison only considers the `x` and `y` coordinates, not the
    * `heading`.
    *
    * @return
    *   An `Option` containing the first revisited `Position` if one exists,
    *   otherwise `None`.
    */
  def findIntersection: Option[Position] =
    history.find(p => history.count(q => p.x == q.x && p.y == q.y) > 1)

/** Traces a path based on a starting position and a list of movement
  * transitions, generating a history of all visited positions.
  *
  * The function simulates movement step-by-step, including intermediate
  * positions. For example, moving "R5" from (0,0) North would generate
  * (0,0,North), (1,0,East), (2,0,East), ..., (5,0,East).
  *
  * @param pos
  *   The initial starting position.
  * @param trajectory
  *   A `List` of [[Transition]] objects representing the sequence of movements.
  * @return
  *   A `List` of all [[Position]]s visited during the trace, including the
  *   starting position and all intermediate steps.
  */
def trace(pos: Position, trajectory: List[Transition]): List[Position] =
  trajectory.foldLeft[List[Position]](List(pos))((pList, t) => {
    val p = pList.last // The current position to start the next segment from

    // Calculate the range of x and y coordinates for the new segment,
    // and determine the new heading after the rotation.
    val (xRange, yRange, newHeading) = t.rotation match
      case Rotation.Right =>
        p.heading match
          case Heading.North =>
            // Facing North, turn Right (East): x increases
            (p.x + 1 to p.x + t.distance by 1, p.y to p.y, Heading.East)
          case Heading.South =>
            // Facing South, turn Right (West): x decreases
            (p.x - 1 to p.x - t.distance by -1, p.y to p.y, Heading.West)
          case Heading.East =>
            // Facing East, turn Right (South): y decreases
            (p.x to p.x, p.y - 1 to p.y - t.distance by -1, Heading.South)
          case Heading.West =>
            // Facing West, turn Right (North): y increases
            (p.x to p.x, p.y + 1 to p.y + t.distance by 1, Heading.North)

      case Rotation.Left =>
        p.heading match
          case Heading.North =>
            // Facing North, turn Left (West): x decreases
            (p.x - 1 to p.x - t.distance by -1, p.y to p.y, Heading.West)
          case Heading.South =>
            // Facing South, turn Left (East): x increases
            (p.x + 1 to p.x + t.distance by 1, p.y to p.y, Heading.East)
          case Heading.East =>
            // Facing East, turn Left (North): y increases
            (p.x to p.x, p.y + 1 to p.y + t.distance by 1, Heading.North)
          case Heading.West =>
            // Facing West, turn Left (South): y decreases
            (p.x to p.x, p.y - 1 to p.y - t.distance by -1, Heading.South)

    // Generate the new segment of positions, excluding the starting point of this segment
    // (as it's already in pList.last) and including the new heading for all positions in the segment.
    val newSegment = for
      x <- xRange
      y <- yRange
    yield Position(x, y, newHeading)

    // Append the new segment's positions to the historical list.
    pList.appendedAll(newSegment)
  })

/** Main application object for Advent of Code 2016 - Day 01.
  *
  * This object extends `ZIOAppDefault`, making it a runnable ZIO application.
  * It reads input from "/01.txt", processes a trajectory of movements,
  * calculates the final distance from the origin, and finds the distance to the
  * first revisited intersection.
  */
object Day01 extends ZIOAppDefault:
  /** The main ZIO effect that constitutes the application's logic.
    *
    * It performs the following steps:
    *   1. Defines the starting position (0,0) facing North. 2. Reads the
    *      trajectory input string from the resource "/01.txt" using
    *      `Util.readBigString`. 3. Parses the input string into a list of
    *      [[Transition]] objects. 4. Traces the entire path, generating a
    *      `List` of all visited [[Position]]s. 5. Calculates the Manhattan
    *      distance of the final position from the origin. 6. Finds the first
    *      intersection (revisited position) in the history and calculates its
    *      distance from the origin. 7. Prints the calculated distances to the
    *      console.
    * @return
    *   A `ZIO` effect that, when executed, performs the day's calculation and
    *   prints results. It yields the distance to the intersection on success.
    */
  def run =
    val startingPosition = Position(0, 0, Heading.North)
    for
      bigString <- Util.readBigString("/01.txt")
      parsedTrajectory = bigString.toTrajectory
      history = trace(startingPosition, parsedTrajectory)
      finalPosition = history.last
      finalDistance = finalPosition.distanceTo(startingPosition)
      intersection = history.findIntersection.getOrElse(finalPosition)
      distanceToIntersection = intersection.distanceTo(startingPosition)
      _ = printf(
        "Day 01\n\tfinal distance: %d\n\tdistance to intersection: %d\n",
        finalDistance,
        distanceToIntersection
      )
    yield distanceToIntersection
