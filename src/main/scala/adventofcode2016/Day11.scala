package adventofcode2016

object Day11:

  enum Element:
    case Pm, Co, Cm, Ru, Pu

  case class Lift(generators: Set[Element], chips: Set[Element])

  case class Floor(number: Byte, maybeLift: Option[Lift], generators: Set[Element], chips: Set[Element])

  type State = Set[Floor]

  type Solution = Seq[State]

  import adventofcode2016.Day11.Element.*

  def findPotentialStates(state: State): Set[State] = ???

  def findSolutions(solutionsFound: Set[Solution], seenStates: Set[State], initialState: State, desiredState: State): Set[Seq[State]] =
    ???

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

    val solutions = findSolutions(Set.empty, Set.empty, initialState, desiredState)

    val minLength = solutions.map(_.size).min

    println(s"Part 1: the minimum number of lift operations is ${minLength}")
