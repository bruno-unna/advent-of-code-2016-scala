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

def trace(pos: Position, trajectory: List[Transition]): List[Position] =
  trajectory.foldLeft[List[Position]](List(pos))((pList, t) => {
    val p = pList.last
    val newSegment = p.heading match
      case Heading.North =>
        if t.rotation == Rotation.Right then
          val x0 = p.x + 1
          val x1 = p.x + t.distance
          x0 to x1 map (x => Position(x, p.y, Heading.East))
        else
          val x0 = p.x - 1
          val x1 = p.x - t.distance
          x0 to x1 by -1 map (x => Position(x, p.y, Heading.West))
      case Heading.South =>
        if t.rotation == Rotation.Right then
          val x0 = p.x - 1
          val x1 = p.x - t.distance
          x0 to x1 by -1 map (x => Position(x, p.y, Heading.West))
        else
          val x0 = p.x + 1
          val x1 = p.x + t.distance
          x0 to x1 map (x => Position(x, p.y, Heading.East))
      case Heading.East =>
        if t.rotation == Rotation.Right then
          val y0 = p.y - 1
          val y1 = p.y - t.distance
          y0 to y1 by -1 map (y => Position(p.x, y, Heading.South))
        else
          val y0 = p.y + 1
          val y1 = p.y + t.distance
          y0 to y1 map (y => Position(p.x, y, Heading.North))
      case Heading.West =>
        if t.rotation == Rotation.Right then
          val y0 = p.y + 1
          val y1 = p.y + t.distance
          y0 to y1 map (y => Position(p.x, y, Heading.North))
        else
          val y0 = p.y - 1
          val y1 = p.y - t.distance
          y0 to y1 by -1 map (y => Position(p.x, y, Heading.South))
    pList.appendedAll(newSegment)
  })

object Day01 extends ZIOAppDefault:
  def run =
    val startingPosition = Position(0, 0, Heading.North)
    for
      bigString <- Util.readBigString("/01.txt")
      parsedTrajectory = bigString.toTrajectory
      newPosition = trace(startingPosition, parsedTrajectory).last
      distance = newPosition.distanceTo(startingPosition)
      _ = printf("Day 01 - distance: %d\n", distance)
    yield distance
