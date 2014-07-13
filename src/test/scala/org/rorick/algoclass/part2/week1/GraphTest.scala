package org.rorick.algoclass.part2.week1

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[Graph]] class.
 */
class GraphTest extends FunSuite with Matchers with PropertyChecks {
  test("should build MST") {
    val data = Table(
      ("graph", "MST"),
      (Graph(), Set.empty[Edge]),
      (Graph(Edge(1, 2, 1)), Set(Edge(1, 2, 1))),
      (Graph(Edge(1, 2, 1), Edge(2, 3, 3), Edge(1, 3, 2)), Set(Edge(1, 2, 1), Edge(1, 3, 2))),
      (Graph(Edge(1, 2, 2), Edge(2, 3, 3), Edge(1, 3, 5)), Set(Edge(1, 2, 2), Edge(2, 3, 3)))
    )

    forAll(data) { (graph, mst) =>
      val actualMst = graph.minimumSpanningTree()
      if (graph.size > 0) {
        actualMst should have size(graph.size - 1)
      } else {
        actualMst should be (empty)
      }

      actualMst should equal(mst)
    }
  }
}
