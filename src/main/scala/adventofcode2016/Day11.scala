package adventofcode2016

object Day11:

  enum Element:
    case Pm, Co, Cm, Ru, Pu

  sealed trait Device

  case class Generator(element: Element) extends Device

  case class Chip(element: Element) extends Device

  case class Floor(number: Byte, withLift: Boolean, devices: Set[Device])

  case class Payload(first: Device, maybeSecond: Option[Device])

  object Payload:
    def from(device: Device): Payload = Payload(device, None)

    def from(aDevice: Device, anotherDevice: Device): Payload =
      val twoDevices = List(aDevice, anotherDevice).sortWith:
        case (Generator(_), Chip(_)) => true
        case (Chip(_), Generator(_)) => false
        case (Generator(a), Generator(b)) => a.ordinal < b.ordinal
        case (Chip(a), Chip(b)) => a.ordinal < b.ordinal
      Payload(twoDevices.head, twoDevices.tail.headOption)

  type State = Set[Floor]

  type Solution = Seq[State]

  import adventofcode2016.Day11.Element.*

  def findSolutions(initialState: State, desiredState: State): Set[Solution] =

    def isSafe(devices: Set[Device]): Boolean =
      val generators = devices.collect { case g: Generator => g.element }
      val chips = devices.collect { case c: Chip => c.element }

      if generators.isEmpty then true
      else chips.forall(c => generators.contains(c))

    def calculateNewState(state: State, originNumber: Byte, destinationNumber: Byte, payload: Payload): Option[State] =
      for
        origin <- state.find(_.number == originNumber)
        destination <- state.find(_.number == destinationNumber)

        newDestinationDevices = payload.maybeSecond.fold(destination.devices + payload.first)(destination.devices + payload.first + _)
        newDestinationFloor <- if isSafe(newDestinationDevices) then Option(Floor(destinationNumber, true, newDestinationDevices)) else None

        newOriginDevices = payload.maybeSecond.fold(origin.devices - payload.first)(origin.devices - payload.first - _)
        newOriginFloor = Floor(originNumber, false, newOriginDevices)

        restOfState = state - origin - destination
        newState = restOfState + newOriginFloor + newDestinationFloor
      yield newState

    def findTransitions(state: State): Set[State] =
      // find the origin floor
      // find all possible payloads (lift with one or two items)
      // find the possible destination floors (can be one or two)

      // for each possible payload:
      //   for each pair of floors (origin, destination):
      //     calculate a new origin (remove lift and items) and destination (include lift and items)
      //     maybe create a new State, with the above pair and the rest of the floors (if it's safe)
      //   yield the newly created maybe state
      // yield a set with all the maybe created states

      // collect the maybe created states

      val originFloor = state.find(_.withLift).get

      val payloadPairs =
        for
          aDevice <- originFloor.devices
          anotherDevice <- originFloor.devices - aDevice
        yield Payload.from(aDevice, anotherDevice)

      val payloads = originFloor.devices.map(Payload.from) ++ payloadPairs

      val destinationFloorNumbers: Set[Byte] =
        originFloor.number match
          case 1 => Set(2)
          case 2 => Set(1, 3)
          case 3 => Set(2, 4)
          case 4 => Set(3)
          case _ => Set.empty

      val maybeStates = for
        payload <- payloads
        destinationNumber <- destinationFloorNumbers
        maybeNewState = calculateNewState(state, originFloor.number, destinationNumber, payload)
      yield maybeNewState

      maybeStates.collect:
        case Some(state) => state

    def loop(solutionsFound: Set[Solution], seenStates: Set[State],
             currentSequence: Seq[State], desiredState: State): Set[Solution] =
      val currentState = currentSequence.last

      if currentState == desiredState then solutionsFound + currentSequence
      else
        val potentialStates: Set[State] = findTransitions(currentState)
        val newSolutions = for
          newState <- potentialStates.diff(seenStates)
          solution <- loop(solutionsFound, seenStates + newState, currentSequence :+ newState, desiredState)
        yield solution
        
        solutionsFound ++ newSolutions

    loop(Set.empty, Set.empty, Seq(initialState), desiredState)

  @main
  def main(): Unit =
    val initialState = Set(
      Floor(1, withLift = true, devices = Set(Generator(Pm), Chip(Pm))),
      Floor(2, withLift = false, devices = Set(Generator(Co), Generator(Cm), Generator(Ru), Generator(Pu))),
      Floor(3, withLift = false, devices = Set(Chip(Co), Chip(Cm), Chip(Ru), Chip(Pu))),
      Floor(4, withLift = false, devices = Set.empty)
    )
    val desiredState = Set(
      Floor(1, withLift = false, devices = Set.empty),
      Floor(2, withLift = false, devices = Set.empty),
      Floor(3, withLift = false, devices = Set.empty),
      Floor(
        4,
        withLift = true,
        devices = Set(
          Generator(Pm), Generator(Co), Generator(Cm), Generator(Ru), Generator(Pu),
          Chip(Pm), Chip(Co), Chip(Cm), Chip(Ru), Chip(Pu)
        )
      )
    )

    val solutions = findSolutions(initialState, desiredState)

    val minLength = solutions.map(_.size).min

    println(s"Part 1: the minimum number of lift operations is ${minLength}")
