package adventofcode2016

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Solves Advent of Code Day 19: An Elephant named Joseph.
 *
 * This problem involves simulating a game of elimination in a circle,
 * specifically the Josephus problem and a modified variant.
 */
object Day19:
  /**
   * Represents an elf in the circle.
   *
   * @param presents The number of presents the elf currently holds.
   * @param neighbour The ID (key) of the next elf in the circle (the elf to steal from in Part 1).
   */
  private case class Elf(presents: Int, neighbour: Int)

  /**
   * Solves Part 1: Elimination is to the immediate left neighbour. (Classic Josephus).
   *
   * The structure is a map-based linked list, allowing O(1) time complexity per round.
   *
   * @param nElves The starting number of elves (IDs from 1 to nElves).
   * @return The ID of the winning elf.
   */
  def play(nElves: Int): Int =

    val playfield = mutable.Map[Int, Elf]()
    1 to nElves foreach (i => playfield.update(i, Elf(1, (i % nElves) + 1)))

    /**
     * Tail-recursive function to simulate the game.
     *
     * @param turn The ID of the elf whose turn it is to steal.
     * @return The ID of the final winning elf.
     */
    @tailrec
    def reduce(turn: Int): Int =
      val elf = playfield(turn)
      if elf.presents == nElves then // we found the winner
        turn
      else // keep playing
        val victim = playfield(elf.neighbour)
        playfield.update(turn, elf.copy(presents = elf.presents + victim.presents, neighbour = victim.neighbour))
        reduce(turn = victim.neighbour)

    reduce(turn = 1)

  /**
   * Solves Part 2: Elimination is the elf directly across the circle.
   *
   * This uses an O(N) approach by maintaining a pointer to the elf immediately preceding the victim.
   *
   * @param nElves The starting number of elves.
   * @return The ID of the winning elf.
   */
  def playDiagonally(nElves: Int): Int =
    val playfield = mutable.Map[Int, Elf]()
    1 to nElves foreach (i => playfield.update(i, Elf(1, (i % nElves) + 1)))

    /**
     * Tail-recursive function to simulate the game with diagonal elimination.
     *
     * The `nearMissIdx` is the ID of the elf immediately before the victim.
     *
     * @param turn The ID of the current elf whose turn it is.
     * @param nElves The number of elves currently remaining in the circle.
     * @param nearMissIdx The ID of the elf immediately preceding the victim.
     * @return The ID of the final winning elf.
     */
    @tailrec
    def reduce(turn: Int, nElves: Int, nearMissIdx: Int): Int =
      if nElves == 1 then turn
      else
        val victimIdx = playfield(nearMissIdx).neighbour
        val victim = playfield(victimIdx)
        val oldElf = playfield(turn)
        val newElf = oldElf.copy(presents = oldElf.presents + victim.presents)
        playfield.update(turn, newElf)

        val oldNearMiss = playfield(nearMissIdx)
        val newNearMiss = oldNearMiss.copy(neighbour = victim.neighbour)
        playfield.update(nearMissIdx, newNearMiss)

        val newNearMissIdx =
          if nElves % 2 == 0 then
            nearMissIdx
          else
            playfield(nearMissIdx).neighbour
        reduce(turn = playfield(turn).neighbour, nElves - 1, newNearMissIdx)

    reduce(turn = 1, nElves = nElves, nearMissIdx = nElves / 2)

  /**
   * The main entry point for Day 19.
   *
   * Calculates and prints the winner for both elimination scenarios.
   */
  @main
  def day19(): Unit =
    val elves = 3004953

    val winner = play(elves)
    println(s"Part 1: out of $elves elves, the winner if elimination is by neighbour is $winner")

    val winnerDiagonally = playDiagonally(elves)
    println(s"Part 2: out of $elves elves, the winner if elimination is diagonal is $winnerDiagonally")