package org.rorick.algoclass.week5

import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[org.rorick.algoclass.week5.DijkstraShortestPath]].
 */
class DijkstraShortestPathTest extends FunSuite with Matchers {
  test("should parse line") {
    val line = "1\t80,982\t163,8164\t170,2620\t145,648\t200,8021\t173,2069\t92,647\t26,4122\t140,546\t11,1913\t160,6461\t27,7905\t40,9047\t150,2183\t61,9146\t159,7420\t198,1724\t114,508\t104,6647\t30,4612\t99,2367\t138,7896\t169,8700\t49,2437\t125,2909\t117,2597\t55,6399"

    val (node, edgesData) = DijkstraShortestPath.parseLine(line)

    node should equal(1)
    edgesData should equal(List(
      80 -> 982, 163 -> 8164, 170 -> 2620, 145 -> 648,
      200 -> 8021, 173 -> 2069, 92 -> 647, 26 -> 4122,
      140 -> 546, 11 -> 1913, 160 -> 6461, 27 -> 7905,
      40 -> 9047, 150 -> 2183, 61 -> 9146, 159 -> 7420,
      198 -> 1724, 114 -> 508, 104 -> 6647, 30 -> 4612,
      99 -> 2367, 138 -> 7896, 169 -> 8700, 49 -> 2437,
      125 -> 2909, 117 -> 2597, 55 -> 6399))
  }
}
