package adventofcode2016

import adventofcode2016.Util
import zio.*

import scala.util.matching.Regex
import scala.annotation.tailrec

type CanReceive = Either[Int, Int] // (left is output, right is bot)

type ValueAssignment = (Int, Int) // (bot id, value)

case class Bot(
    id: Int,
    lowDestination: CanReceive,
    highDestination: CanReceive
)

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
    mailbox: Queue[Int],
    botDef: Bot,
    botQueues: Map[Int, Queue[Int]],
    logQueue: Queue[String],
    chipBuffer: Ref[Option[Int]],
    outputQueues: Map[Int, Queue[Int]]
): UIO[Unit] =
  val process = for
    msg <- mailbox.take
    _ <- logQueue.offer(s"bot ${id} is receiving chip ${msg}")
    bufferedChip <- chipBuffer.get

    _ <- bufferedChip match
      case Some(previousChip) =>
        val (lowChip, highChip) = (previousChip min msg, previousChip max msg)
        for
          _ <-
            if lowChip == 17 && highChip == 61 then
              logQueue.offer(s"*** found bot ${id} ***")
            else ZIO.succeed(true)

          _ <- botDef match
            case Bot(id, Left(lowDestination), Left(highDestination)) =>
              logQueue.offer(
                s"bot ${id} is sending ${lowChip} to output ${lowDestination} and ${highChip} to output ${highDestination}"
              )
            case Bot(id, Left(lowDestination), Right(highDestination)) =>
              logQueue.offer(
                s"bot ${id} is sending ${lowChip} to output ${lowDestination} and ${highChip} to bot ${highDestination}"
              ) *>
                botQueues(highDestination).offer(highChip)
            case Bot(id, Right(lowDestination), Left(highDestination)) =>
              logQueue.offer(
                s"bot ${id} is sending ${lowChip} to bot ${lowDestination} and ${highChip} to output ${highDestination}"
              ) *>
                botQueues(lowDestination).offer(lowChip)
            case Bot(id, Right(lowDestination), Right(highDestination)) =>
              logQueue.offer(
                s"bot ${id} is sending ${lowChip} to bot ${lowDestination} and ${highChip} to bot ${highDestination}"
              ) *>
                botQueues(lowDestination).offer(lowChip) *>
                botQueues(highDestination).offer(highChip)
          _ <- chipBuffer.set(None)
        yield ()
      case None =>
        chipBuffer.set(Some(msg))
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
    _ <- last.set(msg)
    _ <- logQueue.offer(s"output ${id} is receiving chip ${msg}")
  yield ()
  process.forever

def setupBots(
    instructions: Seq[String],
    logQueue: Queue[String]
): UIO[
  (Seq[Fiber[Nothing, Unit]], Map[Int, Queue[Int]], Seq[ValueAssignment])
] =
  val (valInstructions, botInstructions) = instructions.partitionMap:
    instruction =>
      instruction match
        case BotToBotBotRE(botId, lowDst, highDst) =>
          Right((botId.toInt, Right(lowDst.toInt), Right(highDst.toInt)))
        case BotToBotOutRE(botId, lowDst, highDst) =>
          Right((botId.toInt, Right(lowDst.toInt), Left(highDst.toInt)))
        case BotToOutBotRE(botId, lowDst, highDst) =>
          Right((botId.toInt, Left(lowDst.toInt), Right(highDst.toInt)))
        case BotToOutOutRE(botId, lowDst, highDst) =>
          Right((botId.toInt, Left(lowDst.toInt), Left(highDst.toInt)))
        case ValToBotRE(value, botId) => Left((botId.toInt, value.toInt))
  for
    queuesSeq <- ZIO.collectAll(
      botInstructions.map:
        case (id, _, _) =>
          Queue.unbounded[Int].map(id -> _)
    )
    queuesMap = queuesSeq.toMap

    outputs <- ZIO.collectAll(
      (0 to 2) map: id =>
        Queue.unbounded[Int].map(id -> _)
    )
    outputsMap = outputs.toMap
    _ <- ZIO.collectAll(
      outputs.map: output =>
        for
          state <- Ref.make[Int](1)
          actor <- outputActor(output._1, output._2, state, logQueue).forkDaemon
        yield actor
    )

    botFibers <- ZIO.collectAll(
      botInstructions.map:
        case (id, lowDestination, highDestination) =>
          val bot = Bot(id, lowDestination, highDestination)
          for
            state <- Ref.make[Option[Int]](None)
            actor <- botActor(
              id,
              queuesMap(id),
              bot,
              queuesMap,
              logQueue,
              state,
              outputsMap
            ).forkDaemon
            _ <- Console
              .printLine(s"bot ${id} is being created, defined as ${bot}")
              .ignore
          yield actor
    )

  yield (botFibers, queuesMap, valInstructions)

def process(
    botQueues: Map[Int, Queue[Int]],
    valueAssignments: Seq[ValueAssignment],
    logQueue: Queue[String]
): UIO[Unit] =
  ZIO
    .foreach(valueAssignments): va =>
      logQueue.offer(s"offering bot ${va._1} the chip ${va._2}") *>
        botQueues(va._1).offer(va._2)
    .unit

object Day10 extends ZIOAppDefault:

  def run =
    for
      instructions <- Util.readStrings("/10.txt")

      logQueue <- Queue.unbounded[String]
      log <- logActor(logQueue).forkDaemon

      botsSetup <- setupBots(instructions, logQueue)
      (botsFibers, botsQueues, valueAssignments) = botsSetup

      _ <- process(botsQueues, valueAssignments, logQueue)
    yield ExitCode.success
