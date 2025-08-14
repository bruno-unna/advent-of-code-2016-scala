package adventofcode2016

import scala.annotation.tailrec

object Day11:

  enum Element:
    case Pm, Co, Cm, Ru, Pu

  case class Lift(generators: Set[Element], chips: Set[Element])

  case class Floor(number: Byte, maybeLift: Option[Lift], generators: Set[Element], chips: Set[Element])

  type State = Set[Floor]

  type Solution = Seq[State]

  import adventofcode2016.Day11.Element.*

  def findSolutions(initialState: State, desiredState: State): Set[Solution] =

    def isSafe(state:State): Boolean = ???

    def findPotentialStates(state: State): Set[State] = ???

    def loop(solutionsFound: Set[Solution], seenStates: Set[State],
             currentSequence: Seq[State], desiredState: State): Set[Solution] =
      val currentState = currentSequence.last

      if currentState == desiredState then solutionsFound + currentSequence
      else
        val potentialStates: Set[State] = findPotentialStates(currentState).diff(seenStates)
        potentialStates.flatMap: newState =>
          loop(solutionsFound, seenStates + newState, currentSequence.tail, desiredState)

    loop(Set.empty, Set.empty, Seq(initialState), desiredState)

  @main
  def main(): Unit =
    val initialState = Set(
      Floor(1, Some(Lift(Set.empty, Set.empty)), generators = Set(Pm), chips = Set(Pm)),
      Floor(2, None, generators = Set(Co, Cm, Ru, Pu), chips = Set.empty),
      Floor(3, None, generators = Set.empty, chips = Set(Co, Cm, Ru, Pu)),
      Floor(4, None, generators = Set.empty, chips = Set.empty)
    )
    val desiredState = Set(
      Floor(1, None, generators = Set.empty, chips = Set.empty),
      Floor(2, None, generators = Set.empty, chips = Set.empty),
      Floor(3, None, generators = Set.empty, chips = Set.empty),
      Floor(
        4,
        Some(Lift(Set.empty, Set.empty)),
        generators = Set(Pm, Co, Cm, Ru, Pu),
        chips = Set(Pm, Co, Cm, Ru, Pu)
      )
    )

    val solutions = findSolutions(initialState, desiredState)

    val minLength = solutions.map(_.size).min

    println(s"Part 1: the minimum number of lift operations is ${minLength}")
