package adventofcode2016

import zio.{Console, UIO, ZIO, ZIOAppDefault}

import java.io.IOException

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

  def findViablePairs(nodes: Set[Node]): Set[(Pos, Pos)] =
    for
      a <- nodes
      b <- nodes
      if a != b
      if a.used > 0
      if a.used <= b.available
    yield a.pos -> b.pos

  def findShortestPath(space: Set[(Pos, Pos)], origin: (Int, Int), target: (Int, Int)): Seq[(Pos, Pos)] =
    ???

  private def program =
    for
      nodeStrings <- Util.readStrings("/22.txt")
      potentialNodes <- ZIO.foreachPar(nodeStrings)(Node.parse)
      nodes = potentialNodes.collect:
        case Some(n) => n
      viablePairs = findViablePairs(nodes.toSet)
      _ <- Console.printLine(s"part 1: number of viable pairs: ${viablePairs.size}")

      shortestPath = findShortestPath(space = viablePairs, origin = (0, 0), target = (31, 0))
      _ <- Console.printLine(s"part 2: shortest path is of length: ${shortestPath.length}")
    yield ()

  /**
   * The ZIO application run method, providing the live [[Scrambler.Service]].
   */
  override def run: ZIO[Any, IOException, Unit] = program