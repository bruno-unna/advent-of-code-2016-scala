package adventofcode2016

import scala.annotation.tailrec
import scala.collection.mutable

object Day19:
  private case class Elf(presents: Int, neighbour: Int)

  def play(nElves: Int): Int =

    val playfield = mutable.Map[Int, Elf]()
    0 until nElves foreach (i => playfield.update(i, Elf(1, (i + 1) % nElves)))

    @tailrec
    def reduce(turn: Int): Int =
      val elf = playfield(turn)
      if elf.presents == nElves then // we found the winner
        turn
      else // keep playing
        val victim = playfield(elf.neighbour)
        playfield.update(turn, elf.copy(presents = elf.presents + victim.presents, neighbour = victim.neighbour))
        reduce(turn = victim.neighbour)

    reduce(turn = 0) + 1

  @main
  def day19(): Unit =
    val elves = 3004953

    val winner = play(elves)
    println(s"Part 1: out of $elves elves, the winner if elimination is by neighbour is $winner")

