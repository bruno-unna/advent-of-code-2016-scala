package adventofcode2016.day01

import zio._
import adventofcode2016.Util
import scala.util.matching.Regex

enum Heading:
  case North, South, East, West

enum Rotation:
  case Left, Right

case class Position(x: Int, y: Int, heading: Heading):
  def distanceTo(there: Position): Int =
    Math.abs(x - there.x) + Math.abs(y - there.y)

case class Transition(rotation: Rotation, distance: Int)

val TransitionRE: Regex = """\s*(R|L)(\d+)\s*""".r

extension (s: String)
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
  def toTrajectory: List[Transition] =
    s.split(',')
      .map(_.toTransition)
      .filter(_.isDefined)
      .map(_.get)
      .toList

extension (history: List[Position])
  def findIntersection: Option[Position] =
    history.find(p => history.count(q => p.x == q.x && p.y == q.y) > 1)

def trace(pos: Position, trajectory: List[Transition]): List[Position] =
  trajectory.foldLeft[List[Position]](List(pos))((pList, t) => {
    val p = pList.last

    val (xRange, yRange, newHeading) = t.rotation match
      case Rotation.Right =>
        p.heading match
          case Heading.North =>
            (p.x + 1 to p.x + t.distance by 1, p.y to p.y, Heading.East)
          case Heading.South =>
            (p.x - 1 to p.x - t.distance by -1, p.y to p.y, Heading.West)
          case Heading.East =>
            (p.x to p.x, p.y - 1 to p.y - t.distance by -1, Heading.South)
          case Heading.West =>
            (p.x to p.x, p.y + 1 to p.y + t.distance by 1, Heading.North)

      case Rotation.Left =>
        p.heading match
          case Heading.North =>
            (p.x - 1 to p.x - t.distance by -1, p.y to p.y, Heading.West)
          case Heading.South =>
            (p.x + 1 to p.x + t.distance by 1, p.y to p.y, Heading.East)
          case Heading.East =>
            (p.x to p.x, p.y + 1 to p.y + t.distance by 1, Heading.North)
          case Heading.West =>
            (p.x to p.x, p.y - 1 to p.y - t.distance by -1, Heading.South)

    val newSegment = for
      x <- xRange
      y <- yRange
    yield Position(x, y, newHeading)

    pList.appendedAll(newSegment)
  })

object Day01 extends ZIOAppDefault:
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
