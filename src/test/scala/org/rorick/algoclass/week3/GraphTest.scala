package org.rorick.algoclass.week3

import org.scalatest.FunSuite
import org.scalatest.matchers.{MatchResult, Matcher, ShouldMatchers}

/**
 * Test case for [[org.rorick.algoclass.week3.Graph]].
 */
class GraphTest extends FunSuite with ShouldMatchers {

  import Graph.Edge
  import GraphTest.containAll

  test("One node graph") {
    val graph = new Graph(List(List(1)))
    graph.nodes should (contain[Node](SimpleNode(1)) and have length (1))
    graph.edges should be('empty)
    graph.incidentals(SimpleNode(1)) should be('empty)
  }

  test("Two nodes disjoint graph") {
    val graph = new Graph(List(List(1), List(2)))
    graph.nodes should (containAll[Node](SimpleNode(1), SimpleNode(2)) and have length (2))
    graph.edges should be('empty)
    graph.incidentals(SimpleNode(1)) should be('empty)
    graph.incidentals(SimpleNode(2)) should be('empty)
  }

  test("Two nodes connected graph") {
    val graph = new Graph(List(List(1, 2), List(2, 1)))
    graph.nodes should (containAll[Node](SimpleNode(1), SimpleNode(2)) and have length (2))
    graph.edges should (
      containAll[Edge]((SimpleNode(1), SimpleNode(2)), (SimpleNode(2), SimpleNode(1))) and
        have length (2))
    graph.incidentals(SimpleNode(1)) should (contain[Edge](SimpleNode(1), SimpleNode(2)) and have length (1))
    graph.incidentals(SimpleNode(2)) should (contain[Edge](SimpleNode(2), SimpleNode(1)) and have length (1))
  }

  test("Three nodes linear graph") {
    val graph = new Graph(List(List(1, 2), List(2, 3), List(3)))
    graph.nodes should (containAll[Node](SimpleNode(1), SimpleNode(2), SimpleNode(3)) and have length (3))
    graph.edges should (
      containAll[Edge]((SimpleNode(1), SimpleNode(2)), (SimpleNode(2), SimpleNode(3))) and
        have length (2))
    graph.incidentals(SimpleNode(1)) should (contain[Edge](SimpleNode(1), SimpleNode(2)) and have length (1))
    graph.incidentals(SimpleNode(2)) should (contain[Edge](SimpleNode(2), SimpleNode(3)) and have length (1))
    graph.incidentals(SimpleNode(3)) should be('empty)
  }

  test("Three nodes linear undirected graph") {
    val graph = new Graph(List(List(1, 2), List(2, 1, 3), List(3, 2)))
    graph.nodes should (containAll[Node](SimpleNode(1), SimpleNode(2), SimpleNode(3)) and have length (3))
    graph.edges should (
      containAll[Edge](
        (SimpleNode(1), SimpleNode(2)),
        (SimpleNode(2), SimpleNode(1)),
        (SimpleNode(2), SimpleNode(3)),
        (SimpleNode(3), SimpleNode(2)))
        and have length (4))
    graph.incidentals(SimpleNode(1)) should (contain[Edge](SimpleNode(1), SimpleNode(2)) and have length (1))
    graph.incidentals(SimpleNode(2)) should (
      containAll[Edge]((SimpleNode(2), SimpleNode(3)), (SimpleNode(2), SimpleNode(1)))
      and have length (2))
    graph.incidentals(SimpleNode(3)) should (contain[Edge] (SimpleNode(3), SimpleNode(2)))
  }
}

object GraphTest {
  val containAll = new ContainAllWord

  final class ContainAllWord {
    def apply[T](expectedElement: T*): Matcher[Traversable[T]] =
      new Matcher[Traversable[T]] {
        def apply(left: Traversable[T]): MatchResult =
          MatchResult(
            expectedElement.forall(expected => left.exists(e => e == expected)),
            left + " did not contain all expected elements " + expectedElement,
            left + " contained all expected elements " + expectedElement
          )
      }
  }

}
