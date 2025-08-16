package adventofcode2016

import scala.annotation.tailrec

object Day11:

  val nFloors = 4

  enum Element:
    case Pm, Co, Cm, Ru, Pu

  case class Floor(number: Int, withLift: Boolean, generators: Set[Element], chips: Set[Element])

  type State = Set[Floor]

  type Solution = Seq[State]

  import adventofcode2016.Day11.Element.*

  def findSolutions(initialState: State, desiredState: State): Set[Solution] =

    def isSafe(state: State): Boolean = ???

    def findTransitions(state: State): Set[State] =
      // find the origin floor
      // find all possible payloads (lift with one or two items)
      // find the possible destination floors (can be one or two)
      // for each possible payload:
      //   for each pair of floors (origin, destination):
      //     calculate a new origin (remove lift and items) and destination (include lift and items)
      //     maybe create a new State, with the above pair and the rest of the floors (depending on safety)
      //   yield the newly created maybe state
      // yield a set with all the maybe created states
      // collect the maybe created states
      
      val originFloor = state.find(_.withLift).get
      val destinationFloorNumbers =
        ((originFloor.number - 1) :: (originFloor.number + 1) :: Nil)
          .filter(f => f > 0 && f <= nFloors)
      val (destinationFloors, unaffectedFloors) = state.partition(f => destinationFloorNumbers.contains(f.number))
      for
        state <- Set(state)
        floorNumber <- Set(1, 2, 3, 4)
        generator <- state.find(_.withLift)
      yield Set(unaffectedFloors) // TODO add all transitions

    def loop(solutionsFound: Set[Solution], seenStates: Set[State],
             currentSequence: Seq[State], desiredState: State): Set[Solution] =
      val currentState = currentSequence.last

      if currentState == desiredState then solutionsFound + currentSequence
      else
        val potentialStates: Set[State] = findTransitions(currentState).diff(seenStates)
        potentialStates.flatMap: newState =>
          loop(solutionsFound, seenStates + newState, currentSequence.tail, desiredState)

    loop(Set.empty, Set.empty, Seq(initialState), desiredState)

  @main
  def main(): Unit =
    val initialState = Set(
      Floor(1, withLift = true, generators = Set(Pm), chips = Set(Pm)),
      Floor(2, withLift = false, generators = Set(Co, Cm, Ru, Pu), chips = Set.empty),
      Floor(3, withLift = false, generators = Set.empty, chips = Set(Co, Cm, Ru, Pu)),
      Floor(4, withLift = false, generators = Set.empty, chips = Set.empty)
    )
    val desiredState = Set(
      Floor(1, withLift = false, generators = Set.empty, chips = Set.empty),
      Floor(2, withLift = false, generators = Set.empty, chips = Set.empty),
      Floor(3, withLift = false, generators = Set.empty, chips = Set.empty),
      Floor(
        4,
        withLift = true,
        generators = Set(Pm, Co, Cm, Ru, Pu),
        chips = Set(Pm, Co, Cm, Ru, Pu)
      )
    )

    val solutions = findSolutions(initialState, desiredState)

    val minLength = solutions.map(_.size).min

    println(s"Part 1: the minimum number of lift operations is ${minLength}")
