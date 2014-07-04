package org.rorick.algoclass.week4

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[org.rorick.algoclass.week4.Graph]].
 */
class GraphTest extends FunSuite with PropertyChecks with Matchers {
  test("should calculate SCC of zero node graph") {
    new Graph().sccs should have length 0
  }

  test("should calculate SCC of one node graph") {
    val maxNodeNum = 875714
    new Graph((maxNodeNum, maxNodeNum)).sccs should have length 1
  }

  test("should calculate SCC of two node graph with one component") {
    new Graph((1, 2), (2, 1)).sccs should have length 1
  }

  test("should calculate SCC of two node graph with two components") {
    val data = Table(
      ("edges", "number of SCCs"),
      (List((1, 1), (2, 2)), 2),
      (List((1, 2)), 2),
      (List((2, 1)), 2)
    )

    forAll(data) {
      (edges, expectedSccsNum) =>
        new Graph(edges: _*).sccs should have length expectedSccsNum
    }
  }

  test("should calculate SCC of three node graph with one component") {
    val sccs = new Graph((1, 2), (2, 3), (3, 1)).sccs
    sccs should (have length 1)
    sccs(0) should equal(3)
  }

  test("should calculate SCC of three node graph with two components") {
    val sccs = new Graph((1, 2), (2, 1), (1, 3)).sccs
    sccs should (have length (2) and contain (2) and contain (1))
  }

  test("should calculate SCC of three node graph with three components") {
    val sccs = new Graph((1, 1), (2, 2), (3, 3)).sccs
    sccs should (have length (3) and contain (1) and not contain(2) and not contain(3))
  }

  test("should calculate SCCs of lecture graph") {
    val sccs = new Graph((1, 7), (7, 4), (4, 1), (7, 9), (9, 6), (6, 3), (3, 9), (6, 8), (8, 2), (2, 5), (5, 8)).sccs
    sccs should equal (List(3, 3, 3))
  }

  test("should calculate proper reversed graph") {
    val data = Table(
      ("edges", "reversed edges"),
      (List(), List()),
      (List((1, 1)), List((1, 1))),
      (List((1, 2)), List((2, 1))),
      (List((1, 2), (2, 1)), List((2, 1), (1, 2))),
      (List((1, 2), (2, 3)), List((2, 1), (3, 2)))
    )

    forAll(data) {
      (edges, reversedEdges) =>
        new Graph(edges: _*).reversed.edges should equal(reversedEdges)
    }
  }

  test("DFS loop should calculate finishing times") {
    val data = Table(
      ("edges", "finishing times"),
      (List((1, 1)), Map(1 -> 1)),
      (List((1, 2)), Map(1 -> 2, 2 -> 1)),
      (List((2, 1)), Map(1 -> 1, 2 -> 2)),
      (List((1, 2), (2, 3), (3, 1)), Map(1 -> 2, 2 -> 1, 3 -> 3)),
      (List((1, 2), (2, 3)), Map(1 -> 3, 2 -> 2, 3 -> 1)),
      (List((2, 1), (3, 2)), Map(1 -> 1, 2 -> 2, 3 -> 3)),
      (List((1, 2), (1, 3)), Map(1 -> 3, 2 -> 2, 3 -> 1)),
      (List((3, 2), (3, 1)), Map(1 -> 2, 2 -> 1, 3 -> 3))
    )

    forAll(data) {
      (edges, finishingTimes) =>
        val fs = Graph.dfsLoopF(new Graph(edges: _*))
        finishingTimes.keys foreach {
          n =>
            finishingTimes(n) should equal(fs(n))
        }
    }
  }
}
