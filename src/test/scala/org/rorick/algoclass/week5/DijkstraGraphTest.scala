package org.rorick.algoclass.week5

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test case for [[org.rorick.algoclass.week5.DijkstraGraph]].
 */
class DijkstraGraphTest extends FunSuite with ShouldMatchers {
  test("should correctly assemble graph of one node") {
    val graph = new DijkstraGraph

    graph.addNode(1, List())

    graph.nodes should equal(List(1))
    graph.edges should be ('empty)
    graph.incidents(1) should be ('empty)
  }

  test("should correctly assemble graph of two nodes") {
    val graph = new DijkstraGraph

    graph.addNode(1, List(2 -> 5))
    graph.addNode(2, List(1 -> 5))

    graph.nodes should equal(List(1, 2))
    graph.edges should equal(List((1 -> 2, 5), (2 -> 1, 5)))
    graph.incidents(1) should equal(List((1 -> 2, 5)))
    graph.incidents(2) should equal(List((2 -> 1, 5)))
  }

  test("should correctly assemble graph of three nodes") {
    val graph = new DijkstraGraph

    graph.addNode(1, List(2 -> 5, 3 -> 7))
    graph.addNode(2, List(1 -> 5, 3 -> 9))
    graph.addNode(3, List(1 -> 7, 2 -> 9))

    graph.nodes should equal(List(1, 2, 3))
    graph.edges should equal(List((1 -> 2, 5), (1 -> 3, 7), (2 -> 1, 5), (2 -> 3, 9), (3 -> 1, 7), (3 -> 2, 9)))
    graph.incidents(1) should equal(List((1 -> 2, 5), (1 -> 3, 7)))
    graph.incidents(2) should equal(List((2 -> 1, 5), (2 -> 3, 9)))
    graph.incidents(3) should equal(List((3 -> 1, 7), (3 -> 2, 9)))
  }
}
