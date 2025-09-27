package adventofcode2016

import scala.annotation.tailrec

object Day18:

  def findMap(firstRow: String, rows: Int): Vector[String] =

    def nextRow(row: String): String =
      val tiles = row.length
      val chars = row.prepended('.').appended('.').sliding(3).map:
        case "^^." | ".^^" | "^.." | "..^" => '^'
        case _ => '.'
      chars.mkString("")

    @tailrec
    def generate(map: Vector[String], limit: Int): Vector[String] =
      if limit <= 0 then map
      else generate(map :+ nextRow(map.last), limit - 1)

    generate(Vector(firstRow), rows - 1)

  def countSafeTiles(strings: Seq[String]): Int =
    strings.map(_.count(_ == '.')).sum

  @main
  def day18(): Unit =
    val firstRow = "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^.....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^"

    val map = findMap(firstRow = firstRow, rows = 40)
    val safeTiles = countSafeTiles(map)
    println(s"Part 1: there are $safeTiles safe tiles in 40 rows")

    val map2 = findMap(firstRow = firstRow, rows = 400_000)
    val safeTiles2 = countSafeTiles(map2)
    println(s"Part 2: there are $safeTiles2 safe tiles in 400,000 rows")
