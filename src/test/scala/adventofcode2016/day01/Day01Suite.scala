package adventofcode2016.day01

import munit.FunSuite

class Day01Suite extends FunSuite:
  test("example 1"):
    val startingPosition = Position(0, 0, Heading.North)
    val trajectory =
      List(
        Transition(Rotation.Right, 2),
        Transition(Rotation.Left, 3)
      )
    val newPosition = trace(startingPosition, trajectory).last
    val distance = newPosition.distanceTo(startingPosition)
    assertEquals(distance, 5)

  test("example 2"):
    val startingPosition = Position(0, 0, Heading.North)
    val trajectory =
      List(
        Transition(Rotation.Right, 2),
        Transition(Rotation.Right, 2),
        Transition(Rotation.Right, 2)
      )
    val newPosition = trace(startingPosition, trajectory).last
    val distance = newPosition.distanceTo(startingPosition)
    assertEquals(distance, 2)

  test("example 3"):
    val startingPosition = Position(0, 0, Heading.North)
    val trajectory =
      List(
        Transition(Rotation.Right, 5),
        Transition(Rotation.Left, 5),
        Transition(Rotation.Right, 5),
        Transition(Rotation.Right, 3)
      )
    val newPosition = trace(startingPosition, trajectory).last
    val distance = newPosition.distanceTo(startingPosition)
    assertEquals(distance, 12)
