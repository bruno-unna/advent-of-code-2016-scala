package adventofcode2016

import zio.{Console, Scope, UIO, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day22 extends ZIOAppDefault:

  type Pos = (Int, Int)

  case class Node(size: Int, used: Int, available: Int)

  case object Node:
    def parse(s: String): UIO[Option[(Pos, Node)]] =
      val nodeRE = """^/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r
      ZIO.succeed:
        s match
          case nodeRE(x, y, size, used, available, _) =>
            Some((x.toInt, y.toInt) -> Node(size.toInt, used.toInt, available.toInt))
          case _ =>
            None

  case class Mesh(nodes: Map[Pos, Node], accessPos: Pos, interestingPos: Pos, emptyPos: Pos, path: Vector[Pos]):
    private val maxX = nodes.keys.map(_._1).max
    private val maxY = nodes.keys.map(_._2).max

    def neighbours(node: Pos): Set[Pos] =
      Set(
        node.copy(_1 = node._1 - 1),
        node.copy(_1 = node._1 + 1),
        node.copy(_2 = node._2 - 1),
        node.copy(_2 = node._2 + 1))
        .filter(p => p._1 >= 0 && p._1 <= maxX && p._2 >= 0 && p._2 <= maxY)

  def findViablePairs(mesh: Mesh): Iterable[(Node, Node)] =
    val nodes = mesh.nodes.values
    for
      a <- nodes
      b <- nodes
      if a != b
      if a.used > 0
      if a.used <= b.available
    yield a -> b
  end findViablePairs

  def findShortestPath(mesh: Mesh): Vector[Pos] =

    // find the empty node
    // mark the top-right corner node as interesting

    // loop
    // dequeue a mesh
    // if dequeing is not possible, return None
    // else
    // dequeue next mesh
    // if the interesting and access nodes are the same, we're done: return the path so far
    // find the neighbours of the empty node
    // filter out pairs neighbours that have been visited already (???)
    // filter out pairs neighbours if the neighbour has more data than the empty one's size
    // for each remaining neighbour, generate a new mesh changing:
    // - the old empty node is no longer empty (increase its "use" with the neighbour's)
    // - a new empty is now pointing to the neighbour (and make it empty: set its use to 0)
    // - if the neighbour is the interesting node, the interesting node is now the old empty one
    // ... and add the new mesh to the queue
    // invoke recursively, passing:
    // - the queue of meshes
    // - the path so far (increased with the neighbour)
    // - interesting and empty marks
    // - the visited pairs (???)
    @tailrec
    def loop(meshes: Queue[Mesh]): Vector[Pos] =
      if meshes.isEmpty then Vector.empty
      else
        val (mesh, reducedMeshes) = meshes.dequeue
        if mesh.interestingPos == mesh.accessPos then mesh.path
        else
          val neighboursOfEmpty = mesh.neighbours(mesh.emptyPos)
          val validNeighbours = neighboursOfEmpty.filter(pos => mesh.nodes(pos).used <= mesh.nodes(mesh.emptyPos).size)
          val derivedMeshes = validNeighbours.map: neighbourPos =>
            val neighbourNode = mesh.nodes(neighbourPos)
            val emptyNode = mesh.nodes(mesh.emptyPos)
            val newEmptyNode = neighbourNode.copy(used = 0, available = neighbourNode.size)
            val newNeighbourNode = emptyNode.copy(used = neighbourNode.used, available = emptyNode.size - neighbourNode.used)
            val newInterestingPos = if neighbourPos == mesh.interestingPos then
              mesh.emptyPos
            else
              mesh.interestingPos
            mesh.copy(nodes = mesh.nodes.updated(mesh.emptyPos, newNeighbourNode).updated(neighbourPos, newEmptyNode), interestingPos = newInterestingPos, path = mesh.path :+ neighbourPos)
          val newMeshes = meshes.enqueueAll(derivedMeshes)
          loop(newMeshes)
      end if
    end loop

    val queue = Queue[Mesh](mesh)
    loop(queue)
  end findShortestPath

  private def program =
    for
      nodeStrings <- Util.readStrings("/22.txt")
      potentialNodes <- ZIO.foreachPar(nodeStrings)(Node.parse)
      nodes = potentialNodes.collect:
        case Some(n) => n

      emptyPos <- ZIO.fromOption(nodes.collectFirst({ case (pos, node) if node.used == 0 => pos }))
      mesh = Mesh(nodes = nodes.toMap, accessPos = (0, 0), interestingPos = (31, 0), emptyPos = emptyPos, path = Vector(emptyPos))

      viablePairs = findViablePairs(mesh)
      _ <- Console.printLine(s"part 1: number of viable pairs: ${viablePairs.size}")

      shortestPath = findShortestPath(mesh)
      _ <- Console.printLine(s"part 2: shortest path is of length: ${shortestPath.length}")
    yield ()
  end program

  override def run: ZIO[ZIOAppArgs & Scope, Any, Any] = program

end Day22
