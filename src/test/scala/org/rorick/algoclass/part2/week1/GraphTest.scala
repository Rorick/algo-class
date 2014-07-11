package org.rorick.algoclass.part2.week1

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[Graph]] class.
 */
class GraphTest extends FunSuite with Matchers with PropertyChecks {
  test("should build minimum spanning tree") (pendingUntilFixed {
    val data = Table(
      ("graph", "MST"),
      (Graph(), Set.empty[Edge]),
      (Graph(Edge(1, 2, 1)), Set(Edge(1, 2, 1))),
      (Graph(Edge(1, 2, 1), Edge(2, 1, 2)), Set(Edge(1, 2, 1))),
      (Graph(Edge(1, 2, 2), Edge(2, 1, 1)), Set(Edge(2, 1, 1)))
    )

    forAll(data) { (graph, mst) =>
      graph.minimumSpanningTree() should equal(mst)
    }
  })
}
