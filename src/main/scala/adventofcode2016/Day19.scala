package adventofcode2016

import scala.annotation.tailrec
import scala.collection.mutable

object Day19:
  private case class Elf(presents: Int, neighbour: Int)

  def play(nElves: Int): Int =

    val playfield = mutable.Map[Int, Elf]()
    1 to nElves foreach (i => playfield.update(i, Elf(1, (i % nElves) + 1)))

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

  def playDiagonally(nElves: Int): Int =
    val playfield = mutable.Map[Int, Elf]()
    1 to nElves foreach (i => playfield.update(i, Elf(1, (i % nElves) + 1)))

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

  @main
  def day19(): Unit =
    val elves = 3004953

    val winner = play(elves)
    println(s"Part 1: out of $elves elves, the winner if elimination is by neighbour is $winner")

    val winnerDiagonally = playDiagonally(elves)
    println(s"Part 2: out of $elves elves, the winner if elimination is diagonal is $winnerDiagonally")
