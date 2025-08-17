package adventofcode2016

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day11:

  /** Represents a specific element type for a device. */
  enum Element:
    case Pm, Co, Cm, Ru, Pu

  /** A sealed trait representing a generic device. */
  sealed trait Device

  /** A case class representing a generator for a specific element.
   *
   * @param element The element of the generator.
   */
  case class Generator(element: Element) extends Device

  /** A case class representing a microchip for a specific element.
   *
   * @param element The element of the chip.
   */
  case class Chip(element: Element) extends Device

  /** A case class representing a payload of one or two devices to be moved by the lift.
   *
   * @param first       The first device in the payload.
   * @param maybeSecond An optional second device in the payload.
   */
  case class Payload(first: Device, maybeSecond: Option[Device])

  object Payload:

    /** Creates a payload with a single device.
     *
     * @param device The device to be included in the payload.
     * @return A Payload instance with one device.
     */
    def from(device: Device): Payload = Payload(device, None)

    /** Creates a payload with two devices, sorted to ensure a canonical representation.
     *
     * @param aDevice       The first device.
     * @param anotherDevice The second device.
     * @return A Payload instance with two sorted devices.
     */
    def from(aDevice: Device, anotherDevice: Device): Payload =
      val twoDevices = List(aDevice, anotherDevice).sortWith:
        case (Generator(_), Chip(_)) => true
        case (Chip(_), Generator(_)) => false
        case (Generator(a), Generator(b)) => a.ordinal < b.ordinal
        case (Chip(a), Chip(b)) => a.ordinal < b.ordinal
      Payload(twoDevices.head, twoDevices.tail.headOption)

  /** Represents a state of the system, including the lift's position and the location of each device.
   *
   * @param liftFloor       The current floor of the lift.
   * @param deviceLocations A map from each device to its current floor.
   */
  case class State(liftFloor: Byte, deviceLocations: Map[Device, Byte])

  /** A type alias for a sequence of states representing a solution path. */
  type Solution = Seq[State]

  import adventofcode2016.Day11.Element.*

  /** Finds the shortest sequence of states to solve the puzzle using a breadth-first search.
   *
   * @param initialState The starting state of the puzzle.
   * @param desiredState The target state to reach.
   * @return A sequence of states representing the shortest path from the initial to the desired state.
   */
  private def findSolution(initialState: State, desiredState: State): Solution =

    /** Checks if a set of devices on a single floor is in a safe configuration.
     * A floor is safe if there are no generators, or if every microchip is paired with its corresponding generator.
     *
     * @param devices The set of devices on the floor.
     * @return true if the floor is safe, false otherwise.
     */
    def isSafe(devices: Set[Device]): Boolean =
      val generators = devices.collect { case g: Generator => g.element }
      val chips = devices.collect { case c: Chip => c.element }

      generators.isEmpty || chips.forall(c => generators.contains(c))

    /** Calculates a new state after a move, if the move is valid and safe.
     *
     * @param state             The current state of the system.
     * @param destinationNumber The floor the lift is moving to.
     * @param payload           The devices being moved.
     * @return An Option containing the new State if the move is valid, otherwise None.
     */
    def calculateNewState(state: State, destinationNumber: Byte, payload: Payload): Option[State] =
      val newDeviceLocations = payload match
        case Payload(first, Some(second)) => state.deviceLocations + (payload.first -> destinationNumber) + (second -> destinationNumber)
        case Payload(first, None) => state.deviceLocations + (payload.first -> destinationNumber)

      val devicesAtDestination = newDeviceLocations.filter((_, floor) => floor == destinationNumber).keys.toSet
      val devicesAtOrigin = newDeviceLocations.filter((_, floor) => floor == state.liftFloor).keys.toSet

      if isSafe(devicesAtDestination) && isSafe(devicesAtOrigin) then Some(State(destinationNumber, newDeviceLocations))
      else None

    /** Finds all valid next states (transitions) from the current state.
     *
     * @param state The current state.
     * @return A set of all reachable, valid states from the current state.
     */
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
        payload = itemsToMove.headOption match
          case Some(device) => Payload(device, itemsToMove.tail.headOption)
          case None => throw new IllegalStateException("Payload should never be empty")
        newState <- calculateNewState(state, destination, payload)
      yield newState

      validTransitions

    /** Reconstructs the shortest path from the initial to the desired state using a map of parent states.
     *
     * @param parents  A map where the key is a state and the value is its parent state.
     * @param endState The final state of the path.
     * @return A sequence of states representing the path.
     */
    def reconstructPath(parents: Map[State, State], endState: State): Solution =
      @tailrec
      def loop(current: State, acc: List[State]): List[State] =
        parents.get(current) match
          case Some(parent) => loop(parent, current :: acc)
          case None => current :: acc

      loop(endState, Nil)

    /** A tail-recursive breadth-first search algorithm to find the shortest path.
     *
     * @param queue   The queue of states to visit.
     * @param seen    The set of states that have already been visited.
     * @param parents A map to store parent-child relationships for path reconstruction.
     * @return The completed parents map when the desired state is found or the queue is empty.
     */
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

  /** The main entry point for the application. */
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

    val solution = findSolution(initialState, desiredState)

    println(s"Part 1: the minimum number of lift operations is ${solution.length - 1}")