package adventofcode2016

import adventofcode2016.Day22.{Node, findShortestPath, findViablePairs}
import zio.ZIO
import zio.test.{Spec, ZIOSpecDefault, assertTrue}

object Day22Suite extends ZIOSpecDefault:

  val df: Seq[String] = Seq(
    "/dev/grid/node-x0-y0   10T    8T     2T   80%",
    "/dev/grid/node-x0-y1   11T    6T     5T   54%",
    "/dev/grid/node-x0-y2   32T   28T     4T   87%",
    "/dev/grid/node-x1-y0    9T    7T     2T   77%",
    "/dev/grid/node-x1-y1    8T    0T     8T    0%",
    "/dev/grid/node-x1-y2   11T    7T     4T   63%",
    "/dev/grid/node-x2-y0   10T    6T     4T   60%",
    "/dev/grid/node-x2-y1    9T    8T     1T   88%",
    "/dev/grid/node-x2-y2    9T    6T     3T   66%",
  )

  def spec: Spec[Any, String | Throwable] = suite("grid computing")(
    test("find shortest path"):
      for
        potentialNodes <- ZIO.foreachPar(df)(Node.parse)
        nodes = potentialNodes.collect:
          case Some(n) => n
        viablePairs = findViablePairs(nodes.toSet)
        shortestPath = findShortestPath(space = viablePairs, origin = (2, 0), target = (0, 0))
        test <- assertTrue(shortestPath.size == 7)
      yield test
  )
