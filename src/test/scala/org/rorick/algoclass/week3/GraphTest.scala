package org.rorick.algoclass.week3

import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[org.rorick.algoclass.week3.Graph]].
 */
class GraphTest extends FunSuite with Matchers {

  test("One node graph") {
    val graph = new Graph(List(List(1)))
    graph.nodes should contain only SimpleNode(1)
    graph.edges should be(empty)
    graph.incidentals(SimpleNode(1)) should be(empty)
  }

  test("Two nodes disjoint graph") {
    val graph = new Graph(List(List(1), List(2)))
    graph.nodes should contain only(SimpleNode(1), SimpleNode(2))
    graph.edges should be(empty)
    graph.incidentals(SimpleNode(1)) should be(empty)
    graph.incidentals(SimpleNode(2)) should be(empty)
  }

  test("Two nodes connected graph") {
    val graph = new Graph(List(List(1, 2), List(2, 1)))
    graph.nodes should contain only(SimpleNode(1), SimpleNode(2))
    graph.edges should contain only(SimpleNode(1) -> SimpleNode(2), SimpleNode(2) -> SimpleNode(1))
    graph.incidentals(SimpleNode(1)) should contain only(SimpleNode(1) -> SimpleNode(2), SimpleNode(2) -> SimpleNode(1))
    graph.incidentals(SimpleNode(2)) should contain only(SimpleNode(1) -> SimpleNode(2), SimpleNode(2) -> SimpleNode(1))
  }

  test("Three nodes linear graph") {
    val graph = new Graph(List(List(1, 2), List(2, 3), List(3)))
    graph.nodes should contain only(SimpleNode(1), SimpleNode(2), SimpleNode(3))
    graph.edges should contain only(SimpleNode(1) -> SimpleNode(2), SimpleNode(2) -> SimpleNode(3))
    graph.incidentals(SimpleNode(1)) should contain only SimpleNode(1) -> SimpleNode(2)
    graph.incidentals(SimpleNode(2)) should contain only(SimpleNode(1) -> SimpleNode(2), SimpleNode(2) -> SimpleNode(3))
    graph.incidentals(SimpleNode(3)) should contain only SimpleNode(2) -> SimpleNode(3)
  }

  test("Three nodes linear undirected graph") {
    val graph = new Graph(List(List(1, 2), List(2, 1, 3), List(3, 2)))
    graph.nodes should contain only(SimpleNode(1), SimpleNode(2), SimpleNode(3))
    graph.edges should contain only(
      SimpleNode(1) -> SimpleNode(2),
      SimpleNode(2) -> SimpleNode(1),
      SimpleNode(2) -> SimpleNode(3),
      SimpleNode(3) -> SimpleNode(2))
    graph.incidentals(SimpleNode(1)) should contain only(SimpleNode(1) -> SimpleNode(2), SimpleNode(2) -> SimpleNode(1))
    graph.incidentals(SimpleNode(2)) should contain only(
      SimpleNode(2) -> SimpleNode(3),
      SimpleNode(3) -> SimpleNode(2),
      SimpleNode(2) -> SimpleNode(1),
      SimpleNode(1) -> SimpleNode(2))
    graph.incidentals(SimpleNode(3)) should contain only(SimpleNode(3) -> SimpleNode(2), SimpleNode(2) -> SimpleNode(3))
  }

  test("Contract edges") {
    val graph = new Graph(List(List(1, 2, 3, 4), List(2, 1, 3, 4), List(3, 1, 2, 4), List(4, 1, 2, 3)))
    graph.contractNode(0)

    // contracted and a loop
    val merged = new MergedNode(SimpleNode(1), SimpleNode(2))
    graph.edges should (
      contain noneOf(
        SimpleNode(1) -> SimpleNode(2),
        SimpleNode(2) -> SimpleNode(1)) and
      contain allOf(
        merged -> SimpleNode(3),
        merged -> SimpleNode(4),
        SimpleNode(3) -> merged,
        SimpleNode(4) -> merged))

    graph.incidentals(SimpleNode(1)) should be(empty)
    graph.incidentals(SimpleNode(2)) should be(empty)
    graph.incidentals(merged) should contain allOf(merged -> SimpleNode(3), merged -> SimpleNode(4))
    graph.incidentals(SimpleNode(3)) should contain(SimpleNode(3) -> merged)
    graph.incidentals(SimpleNode(4)) should contain(SimpleNode(4) -> merged)
    graph.nodes should contain only(SimpleNode(3), SimpleNode(4), merged)
  }
}
