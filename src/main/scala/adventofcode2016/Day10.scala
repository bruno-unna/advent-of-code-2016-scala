package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Destination(id: Int)
case class Output(id: Int) extends Destination(id)
case class Bot(id: Int) extends Destination(id)

case class BotDefinition(id: Int, low: Destination, high: Destination)

case class ValueAssignment(botId: Int, value: Int)

val BotToBotBotRE: Regex =
  """^bot (\d+) gives low to bot (\d+) and high to bot (\d+)$""".r
val BotToBotOutRE: Regex =
  """^bot (\d+) gives low to bot (\d+) and high to output (\d+)$""".r
val BotToOutBotRE: Regex =
  """^bot (\d+) gives low to output (\d+) and high to bot (\d+)$""".r
val BotToOutOutRE: Regex =
  """^bot (\d+) gives low to output (\d+) and high to output (\d+)$""".r
val ValToBotRE: Regex =
  """value (\d+) goes to bot (\d+)""".r

def botActor(
    id: Int,
    botDef: BotDefinition,
    botQueues: Map[Int, Queue[Int]],
    outputQueues: Map[Int, Queue[Int]],
    logQueue: Queue[String],
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
          _ <-
            if lowChip == 17 && highChip == 61 then
              logQueue.offer(s"Part 1, found bot ${id}")
            else ZIO.succeed(true)

          _ <- botDef match
            case BotDefinition(
                  id,
                  Output(lowDestination),
                  Output(highDestination)
                ) =>
              logQueue.offer(
                s"bot ${id} sending ${lowChip} to output ${lowDestination} and ${highChip} to output ${highDestination}"
              ) *>
                outputQueues(lowDestination).offer(lowChip) *>
                outputQueues(highDestination).offer(highChip)
            case BotDefinition(
                  id,
                  Output(lowDestination),
                  Bot(highDestination)
                ) =>
              logQueue.offer(
                s"bot ${id} sending ${lowChip} to output ${lowDestination} and ${highChip} to bot ${highDestination}"
              ) *>
                outputQueues(lowDestination).offer(lowChip) *>
                botQueues(highDestination).offer(highChip)
            case BotDefinition(
                  id,
                  Bot(lowDestination),
                  Output(highDestination)
                ) =>
              logQueue.offer(
                s"bot ${id} sending ${lowChip} to bot ${lowDestination} and ${highChip} to output ${highDestination}"
              ) *>
                botQueues(lowDestination).offer(lowChip) *>
                outputQueues(highDestination).offer(highChip)
            case BotDefinition(id, Bot(lowDestination), Bot(highDestination)) =>
              logQueue.offer(
                s"bot ${id} sending ${lowChip} to bot ${lowDestination} and ${highChip} to bot ${highDestination}"
              ) *>
                botQueues(lowDestination).offer(lowChip) *>
                botQueues(highDestination).offer(highChip)
          _ <- firstChipRef.set(None)
        yield ()
      case None =>
        firstChipRef.set(Some(msg))
  yield ()
  process.forever

def logActor(mailbox: Queue[String]): UIO[Unit] =
  val process = for
    msg <- mailbox.take
    _ <- Console.printLine(msg).ignore
  yield ()
  process.forever

def outputActor(
    id: Int,
    mailbox: Queue[Int],
    last: Ref[Int],
    logQueue: Queue[String]
): UIO[Unit] =
  val process = for
    msg <- mailbox.take
    _ <- logQueue.offer(s"output ${id} is receiving chip ${msg}")
    _ <- last.set(msg)
  yield ()
  process.forever

def setupBots(
    instructions: Seq[String],
    logQueue: Queue[String]
): UIO[
  (
      Seq[Fiber[Nothing, Unit]],
      Seq[Fiber[Nothing, Unit]],
      Map[Int, Queue[Int]],
      Seq[ValueAssignment],
      Map[Int, Ref[Int]]
  )
] =
  val (valInstructions, botInstructions) = instructions.partitionMap:
    instruction =>
      instruction match
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
    .map: instruction =>
      instruction match
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
        Ref.make[Int](1).map(id -> _)
    )
    outputStateMap = outputStates.toMap

    outputFibers <- ZIO.collectAll(
      outputQueues.map: indexedQueue =>
        for
          actor <- outputActor(
            indexedQueue._1,
            indexedQueue._2,
            outputStateMap(indexedQueue._1),
            logQueue
          ).forkDaemon
        yield actor
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
              logQueue,
              firstChip
            ).forkDaemon
          yield actor
    )
  yield (botFibers, outputFibers, botQueueMap, valInstructions, outputStateMap)

def process(
    botQueues: Map[Int, Queue[Int]],
    valueAssignments: Seq[ValueAssignment],
    logQueue: Queue[String]
): UIO[Unit] =
  ZIO
    .foreach(valueAssignments): va =>
      botQueues(va._1).offer(va._2)
    .unit

object Day10 extends ZIOAppDefault:
  def run =
    for
      instructions <- Util.readStrings("/10.txt")

      logQueue <- Queue.unbounded[String]
      logFiber <- logActor(logQueue).forkDaemon

      botsSetup <- setupBots(instructions, logQueue)
      (botsFibers, outputFibers, botsQueues, valueAssignments, outputStateMap) =
        botsSetup

      _ <- process(
        botsQueues,
        valueAssignments,
        logQueue
      )

      _ <- ZIO.sleep(1.seconds)

      outputValues <- ZIO.collectAll(
        (0 to 2) map (id => outputStateMap(id).get)
      )
      product = outputValues.reduce(_ * _)

      _ <- Console.printLine(
        s"Part 2, the product of outputs 0, 1 and 2 is ${product}"
      )

      _ <- Console.printLine("Simulation time elapsed. Shutting down...").ignore

      _ <- ZIO.foreach(botsFibers)(_.interrupt)
      _ <- ZIO.foreach(outputFibers)(_.interrupt)
      _ <- logFiber.interrupt
    yield ExitCode.success
