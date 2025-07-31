package adventofcode2016

import adventofcode2016.Day10.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object Day10Suite extends ZIOSpecDefault:
  def spec: Spec[Any, Throwable] = suite("Balance Bots Unit Tests")(
    test("small pass of chips"):
      val instructions: List[String] = List(
        "value 5 goes to bot 2",
        "bot 2 gives low to bot 1 and high to bot 0",
        "value 3 goes to bot 1",
        "bot 1 gives low to output 1 and high to bot 0",
        "bot 0 gives low to output 2 and high to output 0",
        "value 2 goes to bot 2"
      )
      for
        logQueue <- Queue.unbounded[String]
        log <- logActor(logQueue).forkDaemon

        botsSetup <- setupBots(instructions, logQueue)
        (botsFibers, outputFibers, botsQueues, valueAssignments) = botsSetup

        _ <- process(botsQueues, valueAssignments, logQueue)
        _ <- Console.printLine("Zzzz...")
        _ <- ZIO.sleep(3.seconds)
        _ <- Console.printLine("Awake!")
        _ = botsFibers.foreach(_.interrupt)
        _ = outputFibers.foreach(_.interrupt)
        _ = log.interrupt
      yield assertTrue(true)
  )
