package adventofcode2016

import zio._
import adventofcode2016.Util
import scala.util.Try

case class Triangle(a: Int, b: Int, c: Int):
  def isValid: Boolean =
    (a + b) > c && (a + c) > b && (b + c) > a

object Day03 extends ZIOAppDefault:
  def stringToTriangle(triangleAsString: String): Option[Triangle] =
    triangleAsString.trim.split("""\s+""") match
      case s @ Array(_, _, _) =>
        Try(s.map(_.toInt)).map(i => Triangle(i(0), i(1), i(2))).toOption
      case _ => None

  def run =
    for
      triangleStrings <- Util.readStrings("/03.txt")
      triangles = triangleStrings
        .map(str => stringToTriangle(str))
        .filter(_.isDefined)
        .map(_.get)
      triangle_count = triangles.count(_.isValid)
      _ = printf(
        "Day 03\n\tvalid triangles: %d\n",
        triangle_count
      )
    yield triangle_count
