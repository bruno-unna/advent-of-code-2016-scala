package adventofcode2016

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day11:

  enum Element:
    case Pm, Co, Cm, Ru, Pu

  sealed trait Device

  case class Generator(element: Element) extends Device

  case class Chip(element: Element) extends Device

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

  case class State(liftFloor: Byte, floors: Map[Byte, Set[Device]])

  type Solution = Seq[State]

  import adventofcode2016.Day11.Element.*

  def findSolutions(initialState: State, desiredState: State): Solution =

    def isSafe(devices: Set[Device]): Boolean =
      val generators = devices.collect { case g: Generator => g.element }
      val chips = devices.collect { case c: Chip => c.element }

      if generators.isEmpty then true
      else chips.forall(c => generators.contains(c))

    def calculateNewState(state: State, originNumber: Byte, destinationNumber: Byte, payload: Payload): Option[State] =
      val originDevices = state.floors(state.liftFloor)
      val destinationDevices = state.floors(destinationNumber)
      val newDestinationDevices = payload.maybeSecond.fold(destinationDevices + payload.first)(destinationDevices + payload.first + _)
      for
        newDestinationDevices <- Some(newDestinationDevices).filter(isSafe)

        newOriginDevices = payload.maybeSecond.fold(originDevices - payload.first)(originDevices - payload.first - _)

        newFloors = state.floors + (originNumber -> newOriginDevices) + (destinationNumber -> newDestinationDevices)
      yield State(destinationNumber, newFloors)

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

      val originFloorDevices = state.floors(state.liftFloor)

      val payloadPairs =
        for
          aDevice <- originFloorDevices
          anotherDevice <- originFloorDevices - aDevice
        yield Payload.from(aDevice, anotherDevice)

      val payloads = originFloorDevices.map(Payload.from) ++ payloadPairs

      val destinationFloorNumbers: Set[Byte] =
        state.liftFloor match
          case 1 => Set(2)
          case 2 => Set(1, 3)
          case 3 => Set(2, 4)
          case 4 => Set(3)
          case _ => Set.empty

      val maybeStates = for
        payload <- payloads
        destinationNumber <- destinationFloorNumbers
        maybeNewState = calculateNewState(state, state.liftFloor, destinationNumber, payload)
      yield maybeNewState

      maybeStates.collect:
        case Some(state) => state

    def reconstructPaths(parents: Map[State, State], desiredState: State): Solution =
      def loop(current: State): List[State] =
        parents.get(current) match
          case Some(parent) => parent :: loop(parent)
          case None => List(current)

      loop(desiredState).reverse

    @tailrec
    def loop(desiredState: State, queue: Queue[State], seen: Set[State], parents: Map[State, State]): Map[State, State] =
      if queue.isEmpty then parents
      else
        val (state, reducedQueue) = queue.dequeue
        if state == desiredState then parents
        else
          val potentialStates = findTransitions(state).diff(seen)
          val newSeenStates = seen ++ potentialStates
          val newQueue = reducedQueue.enqueueAll(potentialStates)
          val newParents = parents ++ potentialStates.map(_ -> state)
          loop(desiredState, newQueue, newSeenStates, newParents)

    val parents = loop(desiredState, Queue[State](initialState), Set.empty[State], Map.empty[State, State])
    reconstructPaths(parents, desiredState)

  @main
  def main(): Unit =
    val initialState =
      State(1, Map(
        1.byteValue -> Set(Generator(Pm), Chip(Pm)),
        2.byteValue -> Set(Generator(Co), Generator(Cm), Generator(Ru), Generator(Pu)),
        3.byteValue -> Set(Chip(Co), Chip(Cm), Chip(Ru), Chip(Pu)),
        4.byteValue -> Set.empty
      ))
    val desiredState =
      State(4, Map(
        1.byteValue -> Set.empty,
        2.byteValue -> Set.empty,
        3.byteValue -> Set.empty,
        4.byteValue -> Set(Generator(Pm), Generator(Co), Generator(Cm), Generator(Ru), Generator(Pu), Chip(Pm), Chip(Co), Chip(Cm), Chip(Ru), Chip(Pu))
      ))

    val solutions = findSolutions(initialState, desiredState)

    println(s"Part 1: the minimum number of lift operations is ${solutions.length}")
