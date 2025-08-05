package adventofcode2016

import adventofcode2016.Util
import zio.*

import java.io.IOException
import scala.util.matching.Regex

/** Represents a destination where a chip can be sent.
  * @param id
  *   The unique identifier of the destination.
  */
trait Destination(id: Int)

/** Represents an output bin, a specific type of chip destination.
  * @param id
  *   The unique identifier of the output bin.
  */
case class Output(id: Int) extends Destination(id)

/** Represents a bot, a specific type of chip destination that can also process chips.
  * @param id
  *   The unique identifier of the bot.
  */
case class Bot(id: Int) extends Destination(id)

/** Defines the behaviour of a bot, specifying where it sends its low and high chips.
  * @param id
  *   The unique identifier of the bot being defined.
  * @param low
  *   The destination for the lower-valued chip.
  * @param high
  *   The destination for the higher-valued chip.
  */
case class BotDefinition(id: Int, low: Destination, high: Destination)

/** Represents an initial assignment of a chip value to a specific bot.
  * @param botId
  *   The identifier of the bot that receives the value.
  * @param value
  *   The chip value to be assigned.
  */
case class ValueAssignment(botId: Int, value: Int)

/** Regular expression to parse instructions where a bot gives low to a bot and high to another bot.
  */
val BotToBotBotRE: Regex = """^bot (\d+) gives low to bot (\d+) and high to bot (\d+)$""".r

/** Regular expression to parse instructions where a bot gives low to a bot and high to an output.
  */
val BotToBotOutRE: Regex = """^bot (\d+) gives low to bot (\d+) and high to output (\d+)$""".r

/** Regular expression to parse instructions where a bot gives low to an output and high to a bot.
  */
val BotToOutBotRE: Regex = """^bot (\d+) gives low to output (\d+) and high to bot (\d+)$""".r

/** Regular expression to parse instructions where a bot gives low to an output and high to another output.
  */
val BotToOutOutRE: Regex = """^bot (\d+) gives low to output (\d+) and high to output (\d+)$""".r

/** Regular expression to parse instructions assigning a value to a bot. */
val ValToBotRE: Regex = """value (\d+) goes to bot (\d+)""".r

/** Defines the behaviour of a single bot actor in the system. Each bot waits for chips, stores the first, and when it
  * receives a second, it processes them (compares, logs if specific values, and sends to destinations).
  *
  * @param id
  *   The unique identifier of this bot actor.
  * @param botDef
  *   The definition object specifying this bot's low and high chip destinations.
  * @param botQueues
  *   A map from bot IDs to their respective message queues, used for sending chips to other bots.
  * @param outputQueues
  *   A map from output IDs to their respective message queues, used for sending chips to outputs.
  * @param firstChipRef
  *   A ZIO Ref to store the first chip received by this bot, waiting for a second.
  * @return
  *   An [[UIO]] representing the perpetual process of this bot actor.
  */
def botActor(
    id: Int,
    botDef: BotDefinition,
    botQueues: Map[Int, Queue[Int]],
    outputQueues: Map[Int, Queue[Int]],
    firstChipRef: Ref[Option[Int]]
): UIO[Unit] =
  val mailbox = botQueues(id)
  val process = for
    msg <- mailbox.take
    firstChip <- firstChipRef.get

    _ <- firstChip match
      case Some(previousChip) =>
        val (lowChip, highChip) = (previousChip min msg, previousChip max msg)
        for
          _ <- ZIO.when(lowChip == 17 && highChip == 61)(Console.printLine(s"Part 1, found bot ${id}").ignore)

          _ <- botDef match
            case BotDefinition(id, Output(lowDestination), Output(highDestination)) =>
              outputQueues(lowDestination).offer(lowChip) <&> outputQueues(highDestination).offer(highChip)
            case BotDefinition(id, Output(lowDestination), Bot(highDestination)) =>
              outputQueues(lowDestination).offer(lowChip) <&> botQueues(highDestination).offer(highChip)
            case BotDefinition(id, Bot(lowDestination), Output(highDestination)) =>
              botQueues(lowDestination).offer(lowChip) <&> outputQueues(highDestination).offer(highChip)
            case BotDefinition(id, Bot(lowDestination), Bot(highDestination)) =>
              botQueues(lowDestination).offer(lowChip) <&> botQueues(highDestination).offer(highChip)
            case BotDefinition(_, _, _) => ZIO.dieMessage("an impossible case was detected")
          _ <- firstChipRef.set(None)
        yield ()
      case None =>
        firstChipRef.set(Some(msg))
  yield ()
  process.forever

/** Defines the behaviour of an output actor. Each output actor waits for chips, logs their reception, and stores the
  * last received chip's value.
  *
  * @param id
  *   The unique identifier of this output actor.
  * @param mailbox
  *   The queue from which this output receives chips.
  * @param last
  *   A ZIO Ref to store the value of the last chip received by this output.
  * @return
  *   An [[UIO]] representing the perpetual process of this output actor.
  */
def outputActor(id: Int, mailbox: Queue[Int], last: Ref[Int]): UIO[Unit] =
  val process = for
    msg <- mailbox.take
    _ <- last.set(msg)
  yield ()
  process.forever

/** Sets up the entire bot and output system based on a sequence of instructions. This function parses instructions,
  * creates necessary message queues, ZIO Refs for state, and forks all bot and output actors as daemon fibers.
  *
  * @param instructions
  *   A sequence of strings, each representing an instruction for bot behavior or chip assignment.
  * @return
  *   An [[UIO]] containing a tuple with:
  *   - A sequence of [[Fiber]]s for all forked bot actors.
  *   - A sequence of [[Fiber]]s for all forked output actors.
  *   - A map from bot IDs to their message queues.
  *   - A sequence of initial chip value assignments.
  *   - A map from output IDs to their [[Ref]]s holding the last received chip value.
  */
def setupBots(instructions: Seq[String]): UIO[
  (Seq[Fiber[Nothing, Unit]], Seq[Fiber[Nothing, Unit]], Map[Int, Queue[Int]], Seq[ValueAssignment], Map[Int, Ref[Int]])
] =
  val (valInstructions, botInstructions) = instructions.partitionMap:
    case BotToBotBotRE(botId, lowDst, highDst) =>
      Right((botId.toInt, Bot(lowDst.toInt), Bot(highDst.toInt)))
    case BotToBotOutRE(botId, lowDst, highDst) =>
      Right((botId.toInt, Bot(lowDst.toInt), Output(highDst.toInt)))
    case BotToOutBotRE(botId, lowDst, highDst) =>
      Right((botId.toInt, Output(lowDst.toInt), Bot(highDst.toInt)))
    case BotToOutOutRE(botId, lowDst, highDst) =>
      Right((botId.toInt, Output(lowDst.toInt), Output(highDst.toInt)))
    case ValToBotRE(value, botId) =>
      Left(ValueAssignment(botId.toInt, value.toInt))

  val maxOutput = botInstructions
    .map:
      case (_, Output(first), Output(second)) => first max second
      case (_, Output(first), _)              => first
      case (_, _, Output(second))             => second
      case _                                  => 0
    .max

  for
    outputQueues <- ZIO.collectAll(
      (0 to maxOutput) map: id =>
        Queue.unbounded[Int].map(id -> _)
    )
    outputQueueMap = outputQueues.toMap

    outputStates <- ZIO.collectAll(
      (0 to maxOutput) map: id =>
        Ref.make[Int](-1).map(id -> _)
    )
    outputStateMap = outputStates.toMap

    outputFibers <- ZIO.collectAll(
      outputQueues.map: iq =>
        outputActor(iq._1, iq._2, outputStateMap(iq._1)).forkDaemon
    )

    botQueues <- ZIO.collectAll(
      botInstructions.map:
        case (id, _, _) =>
          Queue.unbounded[Int].map(id -> _)
    )
    botQueueMap = botQueues.toMap

    botFibers <- ZIO.collectAll(
      botInstructions.map:
        case (id, lowDestination, highDestination) =>
          val botDef = BotDefinition(id, lowDestination, highDestination)
          for
            firstChip <- Ref.make[Option[Int]](None)
            actor <- botActor(
              id,
              botDef,
              botQueueMap,
              outputQueueMap,
              firstChip
            ).forkDaemon
          yield actor
    )
  yield (botFibers, outputFibers, botQueueMap, valInstructions, outputStateMap)

/** Processes the initial value assignments by offering chips to their respective bots' queues.
  * @param botQueues
  *   A map from bot IDs to their message queues.
  * @param valueAssignments
  *   A sequence of initial chip value assignments.
  * @return
  *   An [[UIO]] representing the completion of all initial chip offerings.
  */
def process(botQueues: Map[Int, Queue[Int]], valueAssignments: Seq[ValueAssignment]): UIO[Unit] =
  ZIO
    .foreach(valueAssignments): va =>
      botQueues(va._1).offer(va._2)
    .unit

/** Main application object for Day 10 of Advent of Code 2016. It sets up the bot simulation, runs it, and then
  * calculates the answers for Part 1 and Part 2.
  */
object Day10 extends ZIOAppDefault:

  private def waitForRefs(refs: Iterable[Ref[Int]])(predicate: Int => Boolean): ZIO[Any, Nothing, Unit] =
    ZIO
      .collectAll(refs.map(_.get))
      .repeat(Schedule.spaced(10.milliseconds) && Schedule.recurWhile[Iterable[Int]](!_.forall(predicate)))
      .unit

  /** The main entry point of the ZIO application. Reads instructions, sets up and runs the bot simulation, calculates
    * results for Part 1 and Part 2, and then shuts down all actors.
    * @return
    *   An [[ExitCode]] indicating the application's success or failure.
    */
  def run: ZIO[ZIOAppArgs & Scope, IOException, ExitCode] =
    for
      instructions <- Util.readStrings("/10.txt")

      botsSetup <- setupBots(instructions)
      (botsFibers, outputFibers, botsQueues, valueAssignments, outputStateMap) = botsSetup

      _ <- process(botsQueues, valueAssignments)

      _ <- waitForRefs(outputStateMap.values)(_ >= 0).ensuring:
        ZIO.foreach(botsFibers)(_.interrupt)
        ZIO.foreach(outputFibers)(_.interrupt)

      outputValues <- ZIO.collectAll(
        (0 to 2) map (id => outputStateMap(id).get)
      )
      product = outputValues.product

      _ <- Console.printLine(s"Part 2, the product of outputs 0, 1 and 2 is ${product}").ignore
    yield ExitCode.success
