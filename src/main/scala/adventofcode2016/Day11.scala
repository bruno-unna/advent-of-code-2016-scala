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

  case class State(liftFloor: Byte, deviceLocations: Map[Device, Byte])

  type Solution = Seq[State]

  import adventofcode2016.Day11.Element.*

  private def findSolutions(initialState: State, desiredState: State): Solution =

    def isSafe(devices: Set[Device]): Boolean =
      val generators = devices.collect { case g: Generator => g.element }
      val chips = devices.collect { case c: Chip => c.element }

      if generators.isEmpty then true
      else chips.forall(c => generators.contains(c))

    def calculateNewState(state: State, destinationNumber: Byte, payload: Payload): Option[State] =
      val newDeviceLocations = payload.maybeSecond.fold(
        state.deviceLocations + (payload.first -> destinationNumber)
      )(
        d => state.deviceLocations + (payload.first -> destinationNumber) + (d -> destinationNumber)
      )

      val devicesAtDestination = newDeviceLocations.filter((_, floor) => floor == destinationNumber).keys.toSet
      val devicesAtOrigin = newDeviceLocations.filter((_, floor) => floor == state.liftFloor).keys.toSet

      if isSafe(devicesAtDestination) && isSafe(devicesAtOrigin) then Some(State(destinationNumber, newDeviceLocations))
      else None

    def findTransitions(state: State): Set[State] =
      val originFloorDevices = state.deviceLocations.filter((_, f) => f == state.liftFloor).keys.toSet

      val destinationFloorNumbers: Set[Byte] =
        state.liftFloor match
          case 1 => Set(2)
          case 2 => Set(1, 3)
          case 3 => Set(2, 4)
          case 4 => Set(3)
          case _ => Set.empty

      val validTransitions = for
        destination <- destinationFloorNumbers
        itemsToMove <- originFloorDevices.subsets(1) ++ originFloorDevices.subsets(2)
        payload = itemsToMove.headOption.fold(Payload(null, None)):
          head => Payload(head, itemsToMove.tail.headOption)
        newState <- calculateNewState(state, destination, payload)
      yield newState

      validTransitions

    def reconstructPath(parents: Map[State, State], endState: State): Solution =
      @tailrec
      def loop(current: State, acc: List[State]): List[State] =
        parents.get(current) match
          case Some(parent) => loop(parent, current :: acc)
          case None => current :: acc

      loop(endState, Nil)

    @tailrec
    def bfs(queue: Queue[State], seen: Set[State], parents: Map[State, State]): Map[State, State] =
      if queue.isEmpty then
        parents
      else
        val (state, reducedQueue) = queue.dequeue
        if state == desiredState then
          parents
        else
          val potentialStates = findTransitions(state).diff(seen)
          val newSeen = seen ++ potentialStates
          val newQueue = reducedQueue.enqueueAll(potentialStates)
          val newParents = parents ++ potentialStates.map(s => s -> state)
          bfs(newQueue, newSeen, newParents)

    val parents = bfs(Queue(initialState), Set(initialState), Map.empty)
    reconstructPath(parents, desiredState)

  @main
  def main(): Unit =
    val initialState =
      State(1, Map(
        Generator(Pm) -> 1.byteValue, Chip(Pm) -> 1.byteValue,
        Generator(Co) -> 2.byteValue, Generator(Cm) -> 2.byteValue, Generator(Ru) -> 2.byteValue, Generator(Pu) -> 2.byteValue,
        Chip(Co) -> 3.byteValue, Chip(Cm) -> 3.byteValue, Chip(Ru) -> 3.byteValue, Chip(Pu) -> 3.byteValue
      ))
    val desiredState =
      State(4, Map(
        Generator(Pm) -> 4.byteValue, Generator(Co) -> 4.byteValue, Generator(Cm) -> 4.byteValue, Generator(Ru) -> 4.byteValue, Generator(Pu) -> 4.byteValue,
        Chip(Pm) -> 4.byteValue, Chip(Co) -> 4.byteValue, Chip(Cm) -> 4.byteValue, Chip(Ru) -> 4.byteValue, Chip(Pu) -> 4.byteValue
      ))

    val solutions = findSolutions(initialState, desiredState)

    println(s"Part 1: the minimum number of lift operations is ${solutions.length - 1}")