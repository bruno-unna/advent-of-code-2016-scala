package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.util.matching.Regex

/** Represents a pixel display with mutable state managed by a `Ref`. It
  * provides methods to manipulate the display based on specific commands.
  *
  * @param displayRef
  *   A `Ref` holding the current state of the display as a
  *   `List[List[Boolean]]`. `true` represents a lit pixel, `false` represents
  *   an unlit pixel.
  */
case class Display(private val displayRef: Ref[List[List[Boolean]]]):
  import Display.{RectCommand, RotateColumnCommand, RotateRowCommand}

  /** Converts the current state of the display into a human-readable string.
    * Lit pixels are represented by '#', unlit pixels by ' '.
    *
    * @return
    *   A `UIO[String]` effect that, when executed, yields the string
    *   representation of the display.
    */
  def asString(): UIO[String] =
    for
      m <- displayRef.get
      mPrime = m
        .map: row =>
          row
            .map: elem =>
              if elem then "#" else " "
            .mkString
        .mkString("\n")
    yield mPrime

  /** Counts the total number of lit pixels on the display.
    *
    * @return
    *   A `UIO[Int]` effect that, when executed, yields the count of lit pixels.
    */
  def countLeds: UIO[Int] =
    displayRef.get.map: m =>
      m.flatten.count(_ == true)

  /** Recursively executes a sequence of display commands. Each command modifies
    * the internal state of the display.
    *
    * @param p
    *   The sequence of command strings to execute.
    * @return
    *   An `IO[Exception, Display]` effect that, when executed, yields the
    *   `Display` instance after all commands have been processed. It may fail
    *   with an `Exception` if parsing or execution encounters an issue.
    */
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
                val (left, right) =
                  idxTuple._1.splitAt(idxTuple._1.length - offset)
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
                val (top, bottom) =
                  idxTuple._1.splitAt(idxTuple._1.length - offset)
                bottom ++ top
              else idxTuple._1

            _ <- displayRef.set(mPrime.transpose)
            display <- execute(p.tail)
          yield display
        case _ =>
          ZIO.succeed(this)

/** Companion object for the `Display` class, providing factory methods and
  * command regexes.
  */
object Display:
  /** Regular expression to parse "rect" commands, capturing width and height.
    */
  val RectCommand: Regex = """rect (\d+)x(\d+)""".r

  /** Regular expression to parse "rotate row" commands, capturing row index and
    * offset.
    */
  val RotateRowCommand: Regex = """rotate row y=(\d+) by (\d+)""".r

  /** Regular expression to parse "rotate column" commands, capturing column
    * index and offset.
    */
  val RotateColumnCommand: Regex = """rotate column x=(\d+) by (\d+)""".r

  /** Creates a new `Display` instance with an initial screen state where all
    * pixels are off.
    *
    * @param width
    *   The width of the display.
    * @param height
    *   The height of the display.
    * @return
    *   A `UIO[Display]` effect that, when executed, yields a new `Display`
    *   instance.
    */
  def make(width: Int, height: Int): UIO[Display] =
    val initialMap = List.fill(height, width)(false)
    Ref.make(initialMap).map(Display(_))

/** Main application object for Advent of Code Day 8. It reads program
  * instructions, simulates them on a display, and prints the final display
  * state and the count of lit LEDs.
  */
object Day08 extends ZIOAppDefault:
  /** The width of the simulated display. */
  val SCREEN_WIDTH = 50

  /** The height of the simulated display. */
  val SCREEN_HEIGHT = 6

  /** The main entry point of the ZIO application. Executes the display program
    * from a file and prints results.
    *
    * @return
    *   An `ExitCode` indicating the success or failure of the application.
    */
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
