package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.Try

/** Represents a triangle with three side lengths.
  *
  * @param a
  *   The length of the first side.
  * @param b
  *   The length of the second side.
  * @param c
  *   The length of the third side.
  */
case class Triangle(a: Int, b: Int, c: Int):
  /** Checks if this triangle is valid according to the triangle inequality
    * theorem.
    *
    * A triangle is valid if and only if the sum of the lengths of any two sides
    * is greater than the length of the third side.
    *
    * @return
    *   `true` if the triangle is valid, `false` otherwise.
    */
  def isValid: Boolean =
    (a + b) > c && (a + c) > b && (b + c) > a

/** Companion object for the [[Triangle]] case class, providing factory methods.
  */
object Triangle:
  /** Attempts to parse a string into a [[Triangle]] object.
    *
    * The input string is expected to contain three space-separated integer
    * values representing the side lengths (e.g., " 10 20 30 ").
    *
    * @param triangleAsString
    *   The string to parse.
    * @return
    *   An `Option` containing the parsed [[Triangle]] if successful, otherwise
    *   `None`.
    */
  def fromString(triangleAsString: String): Option[Triangle] =
    triangleAsString.trim.split("""\s+""") match
      case s @ Array(_, _, _) =>
        Try(s.map(_.toInt)).map(i => Triangle(i(0), i(1), i(2))).toOption
      case _ => None

  /** Attempts to create a [[Triangle]] object from an array of integers.
    *
    * The input array must contain exactly three integers representing the side
    * lengths.
    *
    * @param arr
    *   The array of integers.
    * @return
    *   An `Option` containing the parsed [[Triangle]] if the array has length
    *   3, otherwise `None`.
    */
  def fromArray(arr: Array[Int]): Option[Triangle] =
    if arr.length != 3 then None else Some(Triangle(arr(0), arr(1), arr(2)))

/** Main application object for Advent of Code 2016 - Day 03.
  *
  * This object extends `ZIOAppDefault`, making it a runnable ZIO application.
  * It reads triangle specifications from "/03.txt" and solves two challenges:
  *   1. Counts valid triangles by interpreting each line as a horizontal
  *      triangle. 2. Counts valid triangles by interpreting three consecutive
  *      lines as columns, forming vertical triangles.
  */
object Day03 extends ZIOAppDefault:

  /** Parses a string containing space-separated integers into an `Array[Int]`.
    *
    * The input string is expected to contain three space-separated integer
    * values.
    *
    * @param triangleAsString
    *   The string to parse.
    * @return
    *   An `Option` containing an `Array[Int]` if parsing is successful and the
    *   string yields exactly three integers, otherwise `None`.
    */
  def stringToArray(triangleAsString: String): Option[Array[Int]] =
    triangleAsString.trim.split("""\s+""") match
      case s @ Array(_, _, _) =>
        Try(s.map(_.toInt)).toOption
      case _ => None

  /** The main ZIO effect that constitutes the application's logic for Day 03.
    *
    * It performs the following steps:
    *   1. Reads the input triangle strings from the resource "/03.txt" using
    *      `Util.readStrings`. 2. Processes these strings to count valid
    *      triangles based on a horizontal interpretation (Part 1 of the Advent
    *      of Code challenge). 3. Transforms the input to process it as vertical
    *      columns of numbers, then splits these columns into groups of three to
    *      form vertical triangles. 4. Counts valid triangles based on the
    *      vertical interpretation (Part 2 of the challenge). 5. Prints both
    *      counts to the console.
    *
    * @return
    *   A `ZIO` effect that, when executed, performs the day's calculation and
    *   prints results. It yields a tuple containing the counts of horizontal
    *   and vertical valid triangles.
    */
  def run =
    for
      triangleStrings <- Util.readStrings("/03.txt")

      // Part 1: Horizontal Triangles

      horizontalTriangles = triangleStrings
        .map(Triangle.fromString(_))
        .collect { case Some(t) => t }
      horizontalTriangleCount = horizontalTriangles.count(_.isValid)

      // Part 2: Vertical Triangles

      columns = triangleStrings
        .map(stringToArray)
        .collect { case Some(arr) => arr }
        .transpose
      threeArrays = columns.map(
        _.sliding(3, 3)
          .takeWhile(_.length == 3)
          .map(_.toArray[Int])
          .map(Triangle.fromArray(_))
          .collect { case Some(t) => t }
          .toArray[Triangle]
      )
      verticalTriangles = threeArrays.reduce(_ :++ _)
      verticalTriangleCount = verticalTriangles.count(_.isValid)

      _ = printf(
        "Day 03\n\thorizontal valid triangles: %d\n\tvertical valid triangles: %d\n",
        horizontalTriangleCount,
        verticalTriangleCount
      )
    yield (horizontalTriangleCount, verticalTriangleCount)
