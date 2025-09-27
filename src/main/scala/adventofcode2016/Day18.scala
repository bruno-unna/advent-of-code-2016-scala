package adventofcode2016

object Day18:

  def findMap(firstRow: String, rows: Int): Seq[String] =
    ???

  def countSafeTiles(strings: Seq[String]): Int =
    ???

  @main
  def day18(): Unit =
    val firstRow = "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^.....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^"
    val map = findMap(firstRow = firstRow, rows = 40)
    val safeTiles = countSafeTiles(map)
    println(s"Part 1: there are $safeTiles safe tiles in 40 rows")
