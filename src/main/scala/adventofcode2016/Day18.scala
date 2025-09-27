package adventofcode2016

import scala.annotation.tailrec

/**
 * Solves Advent of Code Day 18: Like a Rogue.
 *
 * This problem involves simulating the growth of a grid of safe ('.') and trap ('&Hat;') tiles
 * based on the state of the row immediately above it.
 */
object Day18:

  /**
   * Generates a specified number of rows based on the trap-generation rules.
   *
   * @param firstRow The initial row string.
   * @param rows     The total number of rows to generate, including the `firstRow`.
   * @return A [[Vector]] containing all the generated row strings in order, starting with `firstRow`.
   */
  def findMap(firstRow: String, rows: Int): Vector[String] =

    /**
     * Calculates the next row based on the current row.
     *
     * A tile is a trap ('&Hat;') if the tiles immediately above it are one of four patterns:
     * '&Hat;&Hat;.', '.&Hat;&Hat;', '&Hat;..', or '..&Hat;'. Otherwise, the tile is safe ('.').
     *
     * @param row The string representing the current row.
     * @return The string representing the row immediately below the current one.
     */
    def nextRow(row: String): String =
      val tiles = row.length
      val chars = row.prepended('.').appended('.').sliding(3).map:
        case "^^." | ".^^" | "^.." | "..^" => '^'
        case _ => '.'
      chars.mkString("")

    /**
     * Tail-recursive function to generate the map.
     *
     * The use of [[Vector]] and the `:+` append operator provides efficient, amortised O(1)
     * appends, ensuring the generation is O(N) in time complexity.
     *
     * @param map   The [[Vector]] accumulating the rows generated so far.
     * @param limit The number of additional rows left to generate.
     * @return The complete [[Vector]] of row strings.
     */
    @tailrec
    def generate(map: Vector[String], limit: Int): Vector[String] =
      if limit <= 0 then map
      else generate(map :+ nextRow(map.last), limit - 1)

    generate(Vector(firstRow), rows - 1)

  /**
   * Counts the total number of safe tiles ('.') across all rows in a sequence.
   *
   * @param strings A sequence of row strings.
   * @return The total count of safe tiles.
   */
  def countSafeTiles(strings: Seq[String]): Int =
    strings.map(_.count(_ == '.')).sum

  /**
   * The main entry point for Day 18.
   *
   * Calculates and prints the safe tile count for 40 rows (Part 1) and 400,000 rows (Part 2).
   */
  @main
  def day18(): Unit =
    val firstRow = "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^.....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^"

    val map = findMap(firstRow = firstRow, rows = 40)
    val safeTiles = countSafeTiles(map)
    println(s"Part 1: there are $safeTiles safe tiles in 40 rows")

    val map2 = findMap(firstRow = firstRow, rows = 400_000)
    val safeTiles2 = countSafeTiles(map2)
    println(s"Part 2: there are $safeTiles2 safe tiles in 400,000 rows")