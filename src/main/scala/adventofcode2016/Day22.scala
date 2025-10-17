package adventofcode2016

import zio.{Console, UIO, ZIO, ZIOAppDefault}

import java.io.IOException
import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day22 extends ZIOAppDefault:

  type Pos = (Int, Int)

  case class Node(pos: Pos, size: Int, used: Int, available: Int, use: Int)

  case object Node:
    def parse(s: String): UIO[Option[Node]] =
      val nodeRE = """^/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r
      ZIO.succeed:
        s match
          case nodeRE(x, y, size, used, available, use) =>
            Some(Node((x.toInt, y.toInt), size.toInt, used.toInt, available.toInt, use.toInt))
          case _ =>
            None

  def findViablePairs(nodes: Set[Node]): Set[(Node, Node)] =
    for
      a <- nodes
      b <- nodes
      if a != b
      if a.used > 0
      if a.used <= b.available
    yield a -> b

  def findShortestPath(space: Set[(Node, Node)], origin: Pos, target: Pos): Seq[Pos] =

    @tailrec
    def loop(space: Set[(Node, Node)], queue: Queue[Pos], currentPath: Seq[Pos]): Seq[Pos] =
      if queue.isEmpty then currentPath
      else
        val (destination, reducedQueue) = queue.dequeue
        if destination == target then
          destination +: currentPath
        else
          val (touched, untouched) = space.partition: (a, b) =>
            a.pos == destination &&
              (
                ((a.pos._1 == b.pos._1) && (a.pos._2 - b.pos._2).abs == 1) ||
                  ((a.pos._2 == b.pos._2) && (a.pos._1 - b.pos._1).abs == 1)
                )

          val modifiedPairs = touched.map: (a, b) =>
            (a.copy(used = 0, available = a.size), b.copy(used = a.used, available = b.size - a.used))
          val newSpace = untouched ++ modifiedPairs
          loop(space = newSpace,
            queue = reducedQueue.appendedAll(modifiedPairs.map((a, b) => b.pos)),
            currentPath = destination +: currentPath)

    loop(space = space, queue = Queue(origin), currentPath = Seq.empty[Pos])

  private def program =
    for
      nodeStrings <- Util.readStrings("/22.txt")
      potentialNodes <- ZIO.foreachPar(nodeStrings)(Node.parse)
      nodes = potentialNodes.collect:
        case Some(n) => n
      viablePairs = findViablePairs(nodes.toSet)
      _ <- Console.printLine(s"part 1: number of viable pairs: ${viablePairs.size}")

      shortestPath = findShortestPath(space = viablePairs, origin = (31, 0), target = (0, 0))
      _ <- Console.printLine(s"part 2: shortest path is of length: ${shortestPath.length}")
    yield ()

  override def run: ZIO[Any, IOException, Unit] = program