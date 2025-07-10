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

type Trajectory = List[Transition]

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
  def toTrajectory: Trajectory =
    s.split(',')
      .map(_.toTransition)
      .filter(_.isDefined)
      .map(_.get)
      .toList

def trace(pos: Position, trajectory: Trajectory): Position =
  trajectory.foldLeft[Position](pos)((p, t) => {
    p.heading match
      case Heading.North =>
        if t.rotation == Rotation.Right then
          Position(p.x + t.distance, p.y, Heading.West)
        else Position(p.x - t.distance, p.y, Heading.East)
      case Heading.South =>
        if t.rotation == Rotation.Right then
          Position(p.x - t.distance, p.y, Heading.East)
        else Position(p.x + t.distance, p.y, Heading.West)
      case Heading.East =>
        if t.rotation == Rotation.Right then
          Position(p.x + t.distance, p.y, Heading.North)
        else Position(p.x - t.distance, p.y, Heading.South)
      case Heading.West =>
        if t.rotation == Rotation.Right then
          Position(p.x - t.distance, p.y, Heading.South)
        else Position(p.x + t.distance, p.y, Heading.North)

  })

object Day01 extends ZIOAppDefault:
  def run =
    val startingPosition = Position(0, 0, Heading.North)
    for
      bigString <- Util.readBigString("/01.txt")
      parsedTrajectory = bigString.toTrajectory
      newPosition = trace(startingPosition, parsedTrajectory)
      distance = newPosition.distanceTo(startingPosition)
      _ = printf("Day 01 - distance: %d\n", distance)
    yield distance
