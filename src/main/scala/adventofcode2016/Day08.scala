package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.util.matching.Regex

case class Display(private val displayRef: Ref[List[List[Boolean]]]):
  val RectCommand: Regex = """rect (\d+)x(\d+)""".r
  val RotateRowCommand: Regex = """rotate row y=(\d+) by (\d+)""".r
  val RotateColumnCommand: Regex = """rotate column x=(\d+) by (\d+)""".r

  def asString(): UIO[String] =
    for
      m <- displayRef.get
      mPrime = m
        .map: row =>
          row
            .map: elem =>
              if elem then "#" else "."
            .mkString
        .mkString("\n")
    yield mPrime

  def countLeds: UIO[Int] =
    displayRef.get.map: m =>
      m.reduce(_ ::: _).count(_ == true)

  def execute(p: Seq[String]): IO[Exception, Display] =
    if p.isEmpty then ZIO.succeed(this)
    else
      p.head match
        case RectCommand(w, h) =>
          val width = w.toInt
          val height = h.toInt
          for
            m <- displayRef.get

            topRect = 0 until height map: row =>
              List.fill[Boolean](width)(true) ++ m(row).drop(width)
            mPrime = topRect.toList ++ m.drop(height)

            _ <- displayRef.set(mPrime)
            display <- execute(p.tail)
          yield display
        case RotateRowCommand(r, o) =>
          val row = r.toInt
          val offset = o.toInt
          for
            m <- displayRef.get

            mPrime = m.zipWithIndex.map: idxTuple =>
              if idxTuple._2 == row then
                val (left, right) = idxTuple._1.splitAt(idxTuple._1.length - offset)
                right ++ left
              else idxTuple._1

            _ <- displayRef.set(mPrime)
            display <- execute(p.tail)
          yield display
        case RotateColumnCommand(c, o) =>
          val col = c.toInt
          val offset = o.toInt
          for
            m <- displayRef.get

            mPrime = m.transpose.zipWithIndex.map: idxTuple =>
              if idxTuple._2 == col then
                val (top, bottom) = idxTuple._1.splitAt(idxTuple._1.length - offset)
                bottom ++ top
              else idxTuple._1

            _ <- displayRef.set(mPrime.transpose)
            display <- execute(p.tail)
          yield display
        case _ =>
          ZIO.succeed(this)

object Display:
  def make(width: Int, height: Int): UIO[Display] =
    val initialMap = List.fill(height, width)(false)
    Ref.make(initialMap).map(Display(_))

object Day08 extends ZIOAppDefault:
  val SCREEN_WIDTH = 50
  val SCREEN_HEIGHT = 6

  def run =
    for
      program <- Util.readStrings("/08.txt")

      display <- Display.make(SCREEN_WIDTH, SCREEN_HEIGHT)
      newDisplay <- display.execute(program)

      asString <- newDisplay.asString()
      nLeds <- newDisplay.countLeds

      _ <- Console.printLine(asString)
      _ <- Console.printLine(s"number of leds on after the program: ${nLeds}")
    yield ExitCode.success
